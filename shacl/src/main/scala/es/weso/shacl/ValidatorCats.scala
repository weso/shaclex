package es.weso.shacl
import es.weso.rdf._
import es.weso.rdf.nodes._
import ViolationError._
import cats._, data._
import cats.implicits._
import util.matching._
import showShacl._
import es.weso.mytyping._

/**
 * This validator is implemented directly in Scala using cats library
 */
case class Validator(schema: Schema) {

  import es.weso.checking._
  import Validator._

  object MyChecker extends CheckerCats {
    type Config = RDFReader
    type Env = ShapeTyping
    type Err = ViolationError
    type Evidence = (NodeShape, String)
    type Log = List[Evidence]
    implicit val envMonoid: Monoid[Env] = new Monoid[Env] {
      def combine(e1: Env, e2: Env): Env = e1.combineTyping(e2)
      def empty: Env = Typing.empty
    }
/*    implicit val logCanLog: CanLog[Log] = new CanLog[Log] {
      def log(msg: String): Log =
        throw new Exception(s"Not implemented logCanLog. Msg: $msg")
    } */
    implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
      def combine(l1: Log, l2: Log): Log = l1 ++ l2
      def empty: Log = List()
    }
    implicit val logShow: Show[Log] = new Show[Log] {
      def show(l: Log): String = l.map { case (ns, msg) => s"${ns}: $msg" }.mkString("\n")
    }
    implicit val typingShow: Show[ShapeTyping] = new Show[ShapeTyping] {
      def show(t: ShapeTyping): String = t.toString
    }
  }
  type ShapeChecker = Shape => CheckTyping
  type NodeShapeChecker = (RDFNode, Shape) => CheckTyping

  import MyChecker._

  /**
   * Checks that a node satisfies a shape
   */
  type CheckTyping = Check[ShapeTyping]
  type PropertyChecker = (Attempt, IRI) => CheckTyping
  type NodeChecker = Attempt => RDFNode => CheckTyping

  /**
   * Return all scopeNode declarations which are pairs (n,s) where
   * <p> `n` = node to validate
   * <p> `s` = candidate shape
   */
  def targetNodes: Seq[(RDFNode, Shape)] = {
    schema.targetNodeShapes
  }

  // Fails if there is any error
  def validateAll(rdf: RDFReader): CheckResult[ViolationError, ShapeTyping, Log] = {
    implicit def showPair = new Show[(ShapeTyping, Evidences)] {
      def show(e: (ShapeTyping, Evidences)): String = {
        s"Typing: ${e._1}\n Evidences: ${e._2}"
      }
    }
    runCheck(checkSchemaAll(schema), rdf)
  }

  def runCheck[A: Show](c: Check[A], rdf: RDFReader): CheckResult[ViolationError, A, Log] = {
    val initial: ShapeTyping = Typing.empty
    val r = run(c)(rdf)(initial)
    CheckResult(r)
  }


  /**
   * Checks if all nodes/shapes are valid in a schema
   * Fails if any of them is not correct
   */
  def checkSchemaAll: Schema => CheckTyping = schema => {
    val shapes = schema.shapes
    val results = shapes.map(sh => shapeChecker(sh)).toList
    for {
      ts <- checkAll(results)
      t <- combineTypings(ts)
    } yield t
  }

  /*  def checkSchemaSome: Schema => Check[Schema] = schema => {
    val shapes = schema.shapes
    val results: Seq[Check[Shape]] = shapes.map(sh => shapeChecker(sh))
    val r = checkAll(results)
    r.map(_ => schema)
  } */

  def shapeChecker: ShapeChecker = shape => for {
    t1 <- checkTargetNodes(shape.targetNodes)(shape)
    t2 <- checkTargetClasses(shape.targetClasses)(shape)
    t3 <- checkTargetSubjectsOf(shape.targetSubjectsOf)(shape)
    t4 <- checkTargetObjectsOf(shape.targetObjectsOf)(shape)
    t <- combineTypings(List(t1,t2,t3,t4))
  } yield t

  def checkTargetNodes(nodes: Seq[RDFNode]): ShapeChecker = shape => {
    val nodesShapes = nodes.map(n => nodeShape(n, shape)).toList
    for {
      ts <- checkAll(nodesShapes)
      t <- combineTypings(ts)
    } yield t
  }

  def checkTargetClasses(classes: Seq[RDFNode]): ShapeChecker = shape => {
    for {
      rdf <- getRDF
      nodes = classes.map(cls => findNodesInClass(cls, rdf)).flatten
      nodesShapes = nodes.map(n => nodeShape(n, shape)).toList
      ts <- checkAll(nodesShapes)
      t <- combineTypings(ts)
    } yield t
  }

  def checkTargetSubjectsOf(preds: Seq[IRI]): ShapeChecker = shape => {
    for {
      rdf <- getRDF
      subjects = preds.map(rdf.triplesWithPredicate(_).map(_.subj).toSeq).flatten
      nodesShapes = subjects.map(n => nodeShape(n, shape)).toList
      ts <- checkAll(nodesShapes)
      t <- combineTypings(ts)
    } yield t
  }

  def checkTargetObjectsOf(preds: Seq[IRI]): ShapeChecker = shape => {
    for {
      rdf <- getRDF
      objects = preds.map(rdf.triplesWithPredicate(_).map(_.obj).toSeq).flatten
      nodesShapes = objects.map(n => nodeShape(n, shape)).toList
      ts <- checkAll(nodesShapes)
      t <- combineTypings(ts)
    } yield t
  }

  def findNodesInClass(cls: RDFNode, rdf: RDFReader): List[RDFNode] =
    rdf.getSHACLInstances(cls).toList

  def nodeShape: NodeShapeChecker = {
    case (node, shape) => {
      val cs = shape.constraints.map(checkConstraint).toList
      //      val r = checkAll(cs.map(c => c(Attempt(NodeShape(node, shape), None))(node)))
      for {
        current <- getTyping
        attempt = Attempt(NodeShape(node, shape), None)
        comp = checkAll(cs.map(c => c(attempt)(node)))
        ts <- runLocal(comp, _.addType(node, shape))
        t <- combineTypings(ts)
        t1 <- if (shape.closed)
          checkClosed(shape.ignoredProperties, shape.predicatesInPropertyConstraints)(attempt)(node)
        else ok(t)
      } yield t1
    }
  }

  def checkConstraint(c: Constraint): NodeChecker = {
    c match {
      case pc: PropertyConstraint     => checkPropertyConstraint(pc)
      case pc: PathPropertyConstraint => throw new Exception(s"Non-implemented PathProperty Constraints yet: $pc")
      case nc: NodeConstraint         => checkNodeConstraint(nc)
    }
  }

  def checkNodeConstraint(nc: NodeConstraint): NodeChecker = attempt => node => {
    val components = nc.components
    val checkers: Seq[NodeChecker] = components.map(component2Checker)
    validateNodeCheckers(attempt, checkers)
  }

  def checkPropertyConstraint(pc: PropertyConstraint): NodeChecker = attempt => _ => {
    val predicate = pc.predicate
    val components = pc.components
    val propertyCheckers: Seq[PropertyChecker] = components.map(component2PropertyChecker)
    validatePredicateCheckers(attempt, predicate, propertyCheckers)
  }

  def validateNodeCheckers(attempt: Attempt, cs: Seq[NodeChecker]): Check[ShapeTyping] = {
    val newAttempt = attempt.copy(path = None)
    val xs = cs.map(c => c(newAttempt)(newAttempt.node)).toList
    for {
      ts <- checkAll(xs)
      t <- combineTypings(ts)
    } yield t
  }

  def validatePredicateCheckers(attempt: Attempt, p: IRI, cs: Seq[PropertyChecker]): Check[ShapeTyping] = {
    val newAttempt = attempt.copy(path = Some(p))
    val xs = cs.map(c => c(newAttempt, p)).toList
    for {
      ts <- checkAll(xs)
      t <- combineTypings(ts)
    } yield t
  }

  def component2Checker(c: Component): NodeChecker = attempt => node => {
    c match {
      case NodeKind(k) => nodeKindChecker(k)(attempt)(node)
      case And(shapes) => and(shapes)(attempt)(node)
      case Or(shapes)  => or(shapes)(attempt)(node)
      case Not(shape)  => not(shape)(attempt)(node)
      case _           => unsupportedNodeChecker(s"Node constraint: $c")(attempt)(node)
    }
  }

  def checkValues(ls: List[RDFNode], p: RDFNode => Check[ShapeTyping]): Check[ShapeTyping] = for {
    ts <- checkList(ls, p)
    t <- combineTypings(ts)
  } yield t

  def component2PropertyChecker(c: Component): PropertyChecker =
    (attempt, predicate) => for {
      rdf <- getRDF
      node = attempt.node
      os = rdf.triplesWithSubjectPredicate(node, predicate).map(_.obj).toList
      check: Check[ShapeTyping] = c match {
        case ShapeComponent(s)   => checkValues(os, shapeComponentChecker(s)(attempt))
        case ClassComponent(c)   => checkValues(os, classComponentChecker(c)(attempt))
        case Datatype(d)         => checkValues(os, datatypeChecker(d)(attempt))
        case NodeKind(k)         => checkValues(os, nodeKindChecker(k)(attempt))
        case MinCount(n)         => minCount(n, os, attempt, predicate)
        case MaxCount(n)         => maxCount(n, os, attempt, predicate)
        case MinExclusive(n)     => checkValues(os, minExclusive(n)(attempt))
        case MinInclusive(n)     => checkValues(os, minInclusive(n)(attempt))
        case MaxInclusive(n)     => checkValues(os, maxInclusive(n)(attempt))
        case MaxExclusive(n)     => checkValues(os, maxExclusive(n)(attempt))
        case MinLength(n)        => checkValues(os, minLength(n)(attempt))
        case MaxLength(n)        => checkValues(os, maxLength(n)(attempt))
        case Pattern(p, flags)   => checkValues(os, pattern(p, flags)(attempt))
        case UniqueLang(v)       => checkValues(os, unsupportedNodeChecker("UniqueLang")(attempt))
        case Equals(p)           => checkValues(os, equals(p)(attempt))
        case Disjoint(p)         => checkValues(os, disjoint(p)(attempt))
        case LessThan(p)         => checkValues(os, lessThan(p)(attempt))
        case LessThanOrEquals(p) => checkValues(os, lessThanOrEquals(p)(attempt))
        case And(shapes)         => checkValues(os, and(shapes)(attempt))
        case Or(shapes)          => checkValues(os, or(shapes)(attempt))
        case Not(shape)          => checkValues(os, not(shape)(attempt))
        case HasValue(v)         => checkValues(os, hasValue(v)(attempt))
        case In(values)          => checkValues(os, inChecker(values)(attempt))
        case _                   => throw new Exception(s"Unsupported check $c")
      }
      t <- check
    } yield t

  def shapeComponentChecker(s: Shape): NodeChecker = attempt => node => for {
    typing <- getTyping
    newTyping <- if (typing.getOkValues(node).contains(s))
      getTyping
    else if (typing.getFailedValues(node).contains(s)) {
      err(failedNodeShape(
        node, s, attempt,
        s"Failed because $node doesn't match shape $s")) >>
        getTyping
    } else runLocal(nodeShape(node, s), _.addType(node, s))
    _ <- addEvidence(attempt.nodeShape, s"$node has shape ${s.id.getOrElse("?")}")
  } yield newTyping

  def classComponentChecker(cls: RDFNode): NodeChecker = attempt => node => for {
    rdf <- getRDF
    t <- condition(rdf.hasSHACLClass(node, cls),
      attempt,
      classError(node, cls, attempt),
      s"$node is in class $cls")
  } yield t

  def nodeKindChecker(k: NodeKindType): NodeChecker = attempt => node =>
    k match {
      case IRIKind            => iriChecker(attempt)(node)
      case LiteralKind        => literalChecker(attempt)(node)
      case BlankNodeKind      => blankNodeChecker(attempt)(node)
      case BlankNodeOrIRI     => blankNodeOrIRIChecker(attempt)(node)
      case BlankNodeOrLiteral => blankNodeOrLiteralChecker(attempt)(node)
      case IRIOrLiteral       => iriOrLiteralChecker(attempt)(node)
    }

  def datatypeChecker(d: IRI): NodeChecker = attempt => node =>
    condition(hasDatatype(node, d),
      attempt,
      datatypeError(node, d, attempt),
      s"$node has datatype $d")

  def unsupportedNodeChecker(msg: String): NodeChecker = attempt => node => {
    err(unsupported(node, attempt, msg)) >>
      getTyping
  }

  def iriChecker: NodeChecker = attempt => node => {
    condition(node.isIRI, attempt,
      iriKindError(node, attempt),
      s"$node is an IRI")
  }

  def compareIntLiterals(
    n: Literal,
    f: (Int, Int) => Boolean,
    err: (RDFNode, Attempt, Int) => ViolationError,
    msg: String): NodeChecker = attempt => node => for {
    ctrolValue <- checkNumeric(n, attempt)
    value <- checkNumeric(node, attempt)
    t <- condition(f(ctrolValue, value), attempt,
      err(node, attempt, ctrolValue),
      s"$node satisfies $msg($n)")
  } yield t

  def minExclusive(n: Literal): NodeChecker =
    compareIntLiterals(n, _ < _, minExclusiveError, "minExclusive")

  def minInclusive(n: Literal): NodeChecker =
    compareIntLiterals(n, _ <= _, minInclusiveError, "minInclusive")

  def maxExclusive(n: Literal): NodeChecker =
    compareIntLiterals(n, _ > _, maxExclusiveError, "maxExclusive")

  def maxInclusive(n: Literal): NodeChecker =
    compareIntLiterals(n, _ >= _, maxInclusiveError, "maxInclusive")

  def minLength(n: Int): NodeChecker = attempt => node =>
    condition(node.getLexicalForm.length >= n, attempt,
      minLengthError(node, attempt, n),
      s"$node satisfies minLength($n)")

  def maxLength(n: Int): NodeChecker = attempt => node =>
    condition(node.getLexicalForm.length <= n, attempt,
      maxLengthError(node, attempt, n),
      s"$node satisfies maxLength($n)")

  def pattern(p: String, flags: Option[String]): NodeChecker = attempt => node => for {
    pattern <- getRegex(p, flags)
    t <- condition(regexMatch(pattern, node), attempt,
      patternError(node, attempt, p, flags),
      s"$node satisfies pattern '$p'${flags.getOrElse("")})")
  } yield t

  // TODO: Use xerces implementation of XPath regex instead of Scala's builtin
  def getRegex(p: String, flags: Option[String]): Check[Regex] = {
    ok(new Regex(p))
  }

  def regexMatch(pattern: Regex, n: RDFNode): Boolean = {
    pattern.findFirstIn(n.getLexicalForm).isDefined
  }

  def equals(p: IRI): NodeChecker =
    comparison(p, "equals", equalsError, equalsNode)
  def disjoint(p: IRI): NodeChecker =
    comparison(p, "disjoint", disjointError, disjointNode)
  def lessThan(p: IRI): NodeChecker =
    comparison(p, "lessThan", lessThanError, lessThanNode)
  def lessThanOrEquals(p: IRI): NodeChecker =
    comparison(p, "lessThanOrEquals", lessThanOrEqualsError, lessThanOrEqualNode)

  // TODO: Maybe add a check to see if the nodes are comparable
  // With current definition, if nodes are not comparable, always returns false without raising any error...
  def comparison(p: IRI,
                 name: String,
                 errorMaker: (RDFNode, Attempt, IRI, Set[RDFNode]) => ViolationError,
                 cond: (RDFNode, RDFNode) => Boolean): NodeChecker =
    attempt => node => for {
      rdf <- getRDF
      subject = attempt.node
      vs = rdf.triplesWithSubjectPredicate(subject, p).map(_.obj)
      t <- condition(vs.forall(cond(node, _)), attempt,
        errorMaker(node, attempt, p, vs),
        s"$node satisfies $name $p with values ${vs})")
    } yield t

  def and(shapes: Seq[Shape]): NodeChecker = attempt => node => {
    val es = shapes.map(s => nodeShape(node, s)).toList
    for {
      ts <- checkAll(es)
      t1 <- addEvidence(attempt.nodeShape, s"$node passes and(${shapes.map(_.showId).mkString(",")})")
      t <- combineTypings(t1 +: ts)
    } yield t
  }

  def or(shapes: Seq[Shape]): NodeChecker = attempt => node => {
    val shapesList = shapes.toList
    val rs = shapesList.map(s => {
      println(s"Checking $node with ${s.showId}")
      nodeShape(node, s)
    })
    for {
      t0 <- getTyping
      t1 <- checkSome(rs, orError(node, attempt, shapesList))
      t2 <- addEvidence(attempt.nodeShape, s"$node passes or(${shapes.map(_.showId).mkString(",")})")
      t3 <- combineTypings(Seq(t1, t2))
    } yield t3
  }

  def not(shape: Shape): NodeChecker = attempt => node => {
    val parentShape = attempt.nodeShape.shape
    val check: Check[ShapeTyping] = nodeShape(node, shape)
    val handleError: ViolationError => Check[ShapeTyping] = e => for {
      t1 <- addNotEvidence(NodeShape(node, shape), e,
        s"$node doesn't satisfy ${shape.showId}. Negation declared in ${parentShape.showId}. Error: $e")
      t2 <- addEvidence(attempt.nodeShape, s"$node satisfies not(${shape.showId})")
      t <- combineTypings(List(t1, t2))
    } yield t
    val handleNotError: ShapeTyping => Check[ShapeTyping] = t =>
      err(notError(node, attempt, shape))
    cond(check, handleNotError, handleError)
  }

  def checkNumeric(node: RDFNode, attempt: Attempt): Check[Int] =
    node match {
      case n: IntegerLiteral => ok(n.int)
      case n: DecimalLiteral => ok(n.decimal.toInt)
      case n: DoubleLiteral  => ok(n.double.toInt)
      case _                 => err(notNumeric(node, attempt)) >> ok(0)
    }

  def literalChecker: NodeChecker = attempt => node => {
    condition(node.isLiteral, attempt,
      literalKindError(node, attempt),
      s"$node is a Literal")
  }

  def blankNodeChecker: NodeChecker = nodeShape => node => {
    condition(node.isBNode, nodeShape,
      bNodeKindError(node, nodeShape),
      s"$node is a Blank Node")
  }

  def blankNodeOrIRIChecker: NodeChecker = nodeShape => node => {
    condition(node.isBNode || node.isIRI, nodeShape,
      bNodeOrIRIKindError(node, nodeShape),
      s"$node is a Blank Node or an IRI")
  }

  def blankNodeOrLiteralChecker: NodeChecker = nodeShape => node => {
    condition(node.isBNode || node.isLiteral, nodeShape,
      bNodeOrLiteralKindError(node, nodeShape),
      s"$node is a Blank Node or Literal")
  }

  def iriOrLiteralChecker: NodeChecker = attempt => node => {
    condition(node.isIRI || node.isLiteral, attempt,
      iriOrLiteralKindError(node, attempt),
      s"$node is a IRI or Literal")
  }

  def hasValue(value: Value): NodeChecker = attempt => currentNode => {
    condition(isValue(currentNode, value), attempt,
      hasValueError(currentNode, attempt, value),
      s"Checked $currentNode sh:hasValue $value")
  }

  def inChecker(values: Seq[Value]): NodeChecker = attempt => currentNode => {
    condition(inValues(currentNode, values), attempt,
      inError(currentNode, attempt, values),
      s"Checked $currentNode sh:in $values")
  }

  def minCount(minCount: Int, os: Seq[RDFNode], attempt: Attempt, predicate: IRI): Check[ShapeTyping] = {
    val count = os.size
    val node = attempt.node
    condition(count >= minCount, attempt,
      minCountError(node, attempt, minCount, os.size),
      s"Checked minCount($minCount) for predicate($predicate) on node $node")
  }

  def maxCount(maxCount: Int, os: Seq[RDFNode], attempt: Attempt, predicate: IRI): Check[ShapeTyping] = {
    val count = os.size
    val node = attempt.node
    condition(count <= maxCount, attempt,
      maxCountError(node, attempt, maxCount, count),
      s"Checked maxCount($maxCount) for predicate($predicate) on node $node")
  }

  def checkClosed(ignoredProperties: List[IRI], allowedProperties: List[IRI]): NodeChecker = attempt => node => {
    println(s"Checking closed ${node} ${ignoredProperties}, ${allowedProperties}")
    for {
      rdf <- getRDF
      neighbours = rdf.triplesWithSubject(node)
      predicates = neighbours.map(_.pred).toList
      notAllowed = predicates.diff(ignoredProperties).diff(allowedProperties)
      t <- condition(notAllowed.isEmpty, attempt,
        closedError(node, attempt, allowedProperties, ignoredProperties, notAllowed),
        s"Passes closed condition with predicates $predicates and ignoredProperties $ignoredProperties")
    } yield t
  }

  /**
   * if condition is true adds an evidence, otherwise, raises an error
   * @param condition condition to check
   * @param attempt current validation attempt that is being tried
   * @param error error to raise in case `condition` is false
   * @param evidence evidence to add to `attempt` in case `condition` is true
   */
  def condition(
    condition: Boolean,
    attempt: Attempt,
    error: ViolationError,
    evidence: String): CheckTyping = for {
    _ <- validateCheck(condition, error)
    newTyping <- addEvidence(attempt.nodeShape, evidence)
  } yield newTyping

  def addEvidence(nodeShape: NodeShape, msg: String): Check[ShapeTyping] = {
    for {
      t <- getTyping
      _ <- addLog(List((nodeShape, msg)))
    } yield t.addEvidence(nodeShape.node, nodeShape.shape, msg)
  }

  def addNotEvidence(nodeShape: NodeShape, e: ViolationError, msg: String): Check[ShapeTyping] = {
    val node = nodeShape.node
    val shape = nodeShape.shape
    for {
      t <- getTyping
      _ <- addLog(List((nodeShape, msg)))
    } yield t.addNotEvidence(node, shape, e)
  }
  
  def runLocal[A](c: Check[A], f: ShapeTyping => ShapeTyping): Check[A] =
    local(f)(c)

  def getRDF: Check[RDFReader] = getConfig // ask[Comput,RDFReader]

  def getTyping: Check[ShapeTyping] = getEnv // ask[Comput,ShapeTyping]

  ////////////////////////////////////////////
  /**
   * Checks that `node` is one of `values`
   */
  def inValues(node: RDFNode, values: Seq[Value]): Boolean = {
    values.exists(_.matchNode(node))
  }

  def isValue(node: RDFNode, value: Value): Boolean = {
    value.matchNode(node)
  }

  def hasDatatype(node: RDFNode, d: IRI): Boolean = {
    node match {
      case l: Literal => l.dataType == d
      case _          => false
    }
  }

  // TODO: Refactor the following code...
  // move to SRDF and check SPARQL compatibility
  // SPARQL comparison opetators: https://www.w3.org/TR/sparql11-query/#OperatorMapping
  def equalsNode(n1: RDFNode, n2: RDFNode): Boolean = (n1, n2) match {
    case (l1: Literal, l2: Literal) => l1 == l2
    case (i1: IRI, i2: IRI)         => i1 == i2
    case (b1: BNodeId, b2: BNodeId) => b1 == b2
    case (_, _)                     => false
  }

  def disjointNode(n1: RDFNode, n2: RDFNode): Boolean = n1 != n2
  def lessThanNode(n1: RDFNode, n2: RDFNode): Boolean = (n1, n2) match {
    case (IntegerLiteral(n1), IntegerLiteral(n2)) => n1 < n2
    case (DecimalLiteral(n1), DecimalLiteral(n2)) => n1 < n2
    case (DoubleLiteral(n1), DoubleLiteral(n2)) => n1 < n2
    case (StringLiteral(n1), StringLiteral(n2)) => n1 < n2
    case (DatatypeLiteral(n1, d1), DatatypeLiteral(n2, d2)) => d1 == d2 && n1 < n2
    case (LangLiteral(n1, l1), LangLiteral(n2, l2)) => n1 < n2
    case (i1: IRI, i2: IRI) => i1.str < i2.str
    case (b1: BNodeId, b2: BNodeId) => b1.id < b2.id
    case (_, _) => false
  }
  def lessThanOrEqualNode(n1: RDFNode, n2: RDFNode): Boolean = (n1, n2) match {
    case (IntegerLiteral(n1), IntegerLiteral(n2)) => n1 <= n2
    case (DecimalLiteral(n1), DecimalLiteral(n2)) => n1 <= n2
    case (DoubleLiteral(n1), DoubleLiteral(n2)) => n1 <= n2
    case (StringLiteral(n1), StringLiteral(n2)) => n1 <= n2
    case (DatatypeLiteral(n1, d1), DatatypeLiteral(n2, d2)) => d1 == d2 && n1 <= n2
    case (LangLiteral(n1, l1), LangLiteral(n2, l2)) => n1 <= n2
    case (i1: IRI, i2: IRI) => i1.str <= i2.str
    case (b1: BNodeId, b2: BNodeId) => b1.id <= b2.id
    case (_, _) => false
  }

  def combineTypings(ts: Seq[ShapeTyping]): Check[ShapeTyping] = {
    ok(Typing.combineTypings(ts))
  }

  // TODO
  // Define a more general method?
  // This method should validate some of the nodes/shapes not raising a whole error if one fails, 
  // but collecting the good ones and the errors...
  def checkAny(xs: Seq[Check[(RDFNode, Shape)]]): Check[Seq[(RDFNode, Shape)]] = {
    val zero: Check[Seq[(RDFNode, Shape)]] = ok(Seq())
    def next(
      x: Check[(RDFNode, Shape)],
      rest: Check[Seq[(RDFNode, Shape)]]): Check[Seq[(RDFNode, Shape)]] = ???
    /*      for {
       rs1 <- catchWrong(x.flatMap(v => Seq(x)))(_ => ok(Seq()))
       rs2 <- rest
      } rs1 ++ rs2 */
    xs.foldRight(zero)(next)
  }

}

object Validator {

  def empty = Validator(schema = Schema.empty)

  /* def runCheck[A](c: Check[A], rdf: RDFReader): Xor[NonEmptyList[ViolationError],List[(A,Evidences)]] = {
   val initial : ShapeTyping = Typing.empty 
   val r = c.runState(Evidences.initial).runReader(rdf).runReader(initial).runChoose.runNel.runEval.run
   r
 } */

  type ShapeTyping = Typing[RDFNode,Shape,ViolationError,String]

/*  type Comput = Fx.fx6[
    Reader[RDFReader,?], 
    Reader[ShapeTyping,?],
    State[Evidences,?],
    Choose, 
    Validate[ViolationError, ?], 
    Eval] */

 type Result[A] =  Xor[NonEmptyList[ViolationError],List[(A,Evidences)]]

 def isOK[A](r: Result[A]): Boolean =
    r.isRight && r.toList.isEmpty == false

// type Check[A] = Eff[Comput,A]


}

