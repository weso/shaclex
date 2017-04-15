package es.weso.shacl
import es.weso.rdf._
import es.weso.rdf.nodes._
import ViolationError._
import cats._
import data._
import cats.implicits._
import showShacl._
import es.weso.typing._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.path.SHACLPath
import es.weso.utils.RegEx

/**
 * This validator is implemented directly in Scala using cats library
 */
case class Validator(schema: Schema) extends LazyLogging {

  import es.weso.checking._
  import Validator._

  object MyChecker extends CheckerCats {
    type Config = RDFReader
    type Env = ShapeTyping
    type Err = ViolationError
//    type Evidence = (NodeShapePair, String)
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
      def show(l: Log): String = l.map(_.show).mkString("\n")
    }
    implicit val typingShow: Show[ShapeTyping] = new Show[ShapeTyping] {
      def show(t: ShapeTyping): String = t.toString
    }
  }

  import MyChecker._

  /**
   * Checks that a node satisfies a shape
   */
  type CheckTyping = Check[ShapeTyping]
  type PropertyChecker = (Attempt, SHACLPath) => CheckTyping
  type NodeChecker = Attempt => RDFNode => CheckTyping
  type ShapeChecker = Shape => CheckTyping
  type NodeShapeChecker = (RDFNode, Shape) => CheckTyping

  /**
   * Return all targetNode declarations which are pairs (n,s) where
   * <p> `n` = node to validate
   * <p> `s` = candidate shape
   */
  def targetNodes: Seq[(RDFNode, Shape)] = {
    schema.targetNodeShapes
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
  def checkSchemaAll: CheckTyping = {
    val shapes = schema.shapes
    val results = shapes.map(sh => shapeChecker(sh)).toList
    for {
      ts <- checkAll(results)
      t <- combineTypings(ts)
    } yield t
  }

  def shapeChecker: ShapeChecker = shape => {
    logger.info(s"Checking shape: ${shape.showId}")
    for {
      _ <- addLogMsg(s"Checking shape ${shape.showId}")
      t1 <- checkTargetNodes(shape.targetNodes)(shape)
      t2 <- checkTargetClasses(shape.targetClasses)(shape)
      t3 <- checkTargetSubjectsOf(shape.targetSubjectsOf)(shape)
      t4 <- checkTargetObjectsOf(shape.targetObjectsOf)(shape)
      t <- combineTypings(List(t1,t2,t3,t4))
    } yield t
  }

  def checkTargetNodes(nodes: Seq[RDFNode]): ShapeChecker = shape => {
    val nodesShapes = nodes.map(n => nodeShape(n, shape)).toList
    logger.info(s"Target nodes of ${shape.showId} = ${nodes.mkString(",")}")
    for {
      _ <- addLogMsg(s"Checking targetNode declarations for shape ${shape.showId}. Nodes: ${nodes}")
      ts <- checkAll(nodesShapes)
      t <- combineTypings(ts)
    } yield t
  }

  def checkTargetClasses(classes: Seq[RDFNode]): ShapeChecker = shape => {
    logger.info(s"Target classes of ${shape.showId} = ${classes.map(_.show).mkString(",")}")
    for {
      rdf <- getRDF
      nodes = classes.map(cls => findNodesInClass(cls, rdf)).flatten
      nodesShapes = {
        logger.info(s"Nodes found for shape class: ${shape.showId}: ${nodes.map(_.show).mkString(",")}")
        nodes.map(n => nodeShape(n, shape)).toList
      }
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

  def nodeShapeRef(node: RDFNode, shapeRef:ShapeRef, attempt: Attempt): Check[ShapeTyping] = for {
    shape <- getShapeRef(shapeRef,attempt,node)
    t <- nodeShape(node,shape)
  } yield t

  def nodeShape: NodeShapeChecker = {
   case (node, shape: NodeShape) => {
     logger.info(s"Node $node - NodeShape ${shape.showId}")
     val attempt = Attempt(NodeShapePair(node, ShapeRef(shape.id)), None)
     for {
       t <- runLocal(checkNodeShape(shape)(attempt)(node), _.addType(node, shape))
     } yield {
       logger.info(s"Result of node $node - NodeShape ${shape.showId}: ${showResult(t)}")
       t
     }
    }
   case (node, ps: PropertyShape) => {
    logger.info(s"Node $node - PropertyShape ${ps.showId}")
    val path = ps.path
    val attempt = Attempt(NodeShapePair(node, ShapeRef(ps.id)), Some(path))
    val cs = ps.components.toList.map(component2PropertyChecker(_,ps)(attempt,path))
    val pss = ps.propertyShapes.toList.map(checkPropertyShapePath(_, path)(attempt)(node))
    for {
      ts1 <- runLocal(checkAll(cs), _.addType(node, ps))
      ts2 <- runLocal(checkAll(pss), _.addType(node, ps))
      t <- combineTypings(ts1 ++ ts2)
      // TODO: Check closed?
    } yield {
      logger.info(s"Result of node $node - PropertyShape ${ps.showId}: ${showResult(t)}")
      t
    }
  }
  }

  def checkNodeShape(shape: Shape): NodeChecker = attempt => node => {
    logger.info(s"checkNodeShape($node,${shape.showId})")
    for {
      ts1 <- checkComponents(shape.components.toList)(attempt)(node)
      ts2 <- checkPropertyShapes(shape.propertyShapes.toList)(attempt)(node)
      t <- combineTypings(Seq(ts1,ts2))
      t1 <- if (shape.closed) for {
        predicates <- predicatesInPropertyConstraints(shape,attempt,node)
        c <- checkClosed(shape.ignoredProperties, predicates) (attempt) (node)
      } yield c
      else ok(t)
    } yield {
      logger.info(s"Result of checkNodeShape($node,${shape.showId} = ${showResult(t1)}")
      t1
    }
  }

  def predicatesInPropertyConstraints(shape: Shape, attempt: Attempt, node: RDFNode): Check[List[IRI]] = for {
    shapes <- getPropertyShapeRefs(shape.propertyShapes.toList,attempt,node)
  } yield shapes.map(_.predicate)


  // TODO. Does it validate property shapes of a property shape?
  def checkPropertyShape(ps: PropertyShape): NodeChecker = attempt => node => {
    logger.info(s"chechPropertyShape($node,${ps.showId})")
    val path = ps.path
    val newAttempt = Attempt(nodeShape = NodeShapePair(node,ShapeRef(ps.id)), Some(path))
    val components = ps.components

    // TODO: Not 100% sure if ps argument is right
    val propertyCheckers: Seq[PropertyChecker] = components.map(component2PropertyChecker(_,ps))

    for {
      t <- validatePathCheckers(newAttempt, path, propertyCheckers)
    } yield {
      logger.info(s"Result of chechPropertyShape($node,${ps.showId}=${showResult(t)}")
      t
    }
  }

  def checkPropertyShapePath(sref: ShapeRef, path: SHACLPath): NodeChecker = attempt => node => for {
    ps <- getPropertyShapeRef(sref,attempt,node)
    rdf <- getRDF
    os = rdf.getValuesFromPath(node,path).toList
    ts <- checkAll(os.map(o => {
      val newAttempt = Attempt(NodeShapePair(o,ShapeRef(ps.id)),Some(path))
      checkPropertyShape(ps)(newAttempt)(o)
    }))
    t <- combineTypings(ts)
  } yield t


  def checkPropertyShapes(shapeRefs: List[ShapeRef]): NodeChecker = attempt => node => {
    logger.info(s"Check propertyShapes($node, ${shapeRefs.map(_.showId).mkString(",")})")
    for {
      pss <- getPropertyShapeRefs(shapeRefs, attempt,node)
      ts <- checkAll(pss.map(checkPropertyShape(_)(attempt)(node)))
      t <- combineTypings(ts)
    } yield {
      logger.info(s"Result of check propertyShapes($node, ${shapeRefs.map(_.showId).mkString(",")})=${showResult(t)}")
      t
    }
  }

  def checkComponents(cs: List[Component]): NodeChecker = attempt => node => for {
    ts <- checkAll(cs.map(checkComponent(_)(attempt)(node)))
    t <- combineTypings(ts)
  } yield t

  def checkComponent(c: Component): NodeChecker = component2Checker(c)

  def validateNodeCheckers(attempt: Attempt, cs: Seq[NodeChecker]): Check[ShapeTyping] = {
    val newAttempt = attempt.copy(path = None)
    val xs = cs.map(c => c(newAttempt)(newAttempt.node)).toList
    for {
      ts <- checkAll(xs)
      t <- combineTypings(ts)
    } yield t
  }

  def validatePathCheckers(attempt: Attempt, path: SHACLPath, cs: Seq[PropertyChecker]): Check[ShapeTyping] = {
    val newAttempt = attempt.copy(path = Some(path))
    val xs = cs.map(c => c(newAttempt, path)).toList
    for {
      ts <- checkAll(xs)
      t <- combineTypings(ts)
    } yield t
  }

  def component2Checker(c: Component): NodeChecker = attempt => node => {
    c match {
      case NodeComponent(s) => nodeComponentChecker(s)(attempt)(node)
      case Datatype(d) => datatypeChecker(d)(attempt)(node)
      case NodeKind(k) => nodeKindChecker(k)(attempt)(node)
      case MinExclusive(n) => minExclusive(n)(attempt)(node)
      case MaxExclusive(n) => maxExclusive(n)(attempt)(node)
      case MinInclusive(n) => minInclusive(n)(attempt)(node)
      case MaxInclusive(n) => maxInclusive(n)(attempt)(node)
      case MinLength(n) => minLength(n)(attempt)(node)
      case MaxLength(n) => maxLength(n)(attempt)(node)
      case Pattern(s,flags) => pattern(s,flags)(attempt)(node)
      case LanguageIn(langs) => languageIn(langs)(attempt)(node)
      case Equals(p) => equals(p)(attempt)(node)
      case Disjoint(p) => disjoint(p)(attempt)(node)
      case LessThan(p) => lessThan(p)(attempt)(node)
      case LessThanOrEquals(p) => lessThanOrEquals(p)(attempt)(node)
      case Xone(shapes) => xone(shapes)(attempt)(node)
      case And(shapes) => and(shapes)(attempt)(node)
      case Or(shapes)  => or(shapes)(attempt)(node)
      case Not(shape)  => not(shape)(attempt)(node)
      case ClassComponent(cls) => classComponentChecker(cls)(attempt)(node)
      case HasValue(v) => hasValue(v)(attempt)(node)
      case In(ls) => inChecker(ls)(attempt)(node)
      case _           => unsupportedNodeChecker(s"Node constraint: $c")(attempt)(node)
    }
  }

  def checkValues(ls: List[RDFNode], p: RDFNode => CheckTyping): CheckTyping = for {
    ts <- checkList(ls, p)
    t <- combineTypings(ts)
  } yield t

  def component2PropertyChecker(c: Component, p: PropertyShape): PropertyChecker = (attempt, path) => {
      for {
        rdf <- getRDF
        node = attempt.node
        os = rdf.getValuesFromPath(node,path).toList
        // rdf.triplesWithSubjectPredicate(node, predicate).map(_.obj).toList
        check: Check[ShapeTyping] = c match {
          case MinCount(n) => minCount(n, os, attempt, path)
          case MaxCount(n) => maxCount(n, os, attempt, path)
          case UniqueLang(v) => uniqueLang(v, os, attempt, path)
          case QualifiedValueShape(shape,min,max,disjoint) =>
            qualifiedValueShape(shape, p, min, max, disjoint, os, attempt, path)
          case HasValue(v) => {
            logger.info(s"HasValuePropertyChecker(v = $v, os=$os, node = $node, path= $path")
            hasValuePropertyChecker(v, os, attempt, node, path)
          }
          case _ => checkValues(os, component2Checker(c)(attempt)(_))
        }
        t <- check
      } yield t
  }

  def nodeComponentChecker(sref: ShapeRef): NodeChecker = attempt => node => {
    for {
      s <- getShapeRef(sref,attempt,node)
      typing <- getTyping
      shape <- getShapeRef(sref, attempt, node)
      newTyping <- if (typing.getOkValues(node).contains(s))
        getTyping
      else if (typing.getFailedValues(node).contains(s)) {
        err(failedNodeShape(
          node, s, attempt,
          s"Failed because $node doesn't match shape $s")) >>
          getTyping
      } else runLocal(nodeShape(node, s), _.addType(node, s))
      _ <- addEvidence(attempt, s"$node has shape ${s.id}")
    } yield newTyping
  }

  def classComponentChecker(cls: RDFNode): NodeChecker = attempt => node => {
    for {
      rdf <- getRDF
      t <- condition(rdf.hasSHACLClass(node, cls),
        attempt,
        classError(node, cls, attempt),
        s"$node is in class $cls")
    } yield t
  }

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
    b <- regexMatch(p,flags,node.getLexicalForm,node,attempt)
    t <- condition(b, attempt,patternError(node, attempt, p, flags),
      s"$node satisfies pattern ~/$p/${flags.getOrElse("")}")
  } yield t

  def regexMatch(p: String, flags: Option[String], str: String, node:RDFNode, attempt: Attempt): Check[Boolean] =
    RegEx(p,flags).matches(str) match {
      case Left(msg) => err(regexError(node,attempt,msg))
      case Right(b) => ok(b)
    }

  def uniqueLang(b: Boolean, os: Seq[RDFNode], attempt: Attempt, path: SHACLPath): Check[ShapeTyping] = if (b) {
    val node = attempt.node
    for {
      t <- condition(checkUniqueLang(os), attempt,
        uniqueLangError(node, attempt, path, os),
        s"Checked uniqueLang(true) for path $path on node $node")
    } yield t
  } else getTyping

  def checkUniqueLang(os: Seq[RDFNode]): Boolean = {
    def getLanguageTag(n: RDFNode): Option[String] = {
      n match {
        case LangLiteral(_,l) => Some(l.lang)
        case _ => None
      }
    }
    val langs : Seq[String] = os.map(getLanguageTag).flatten

    // If there are duplicated langs, the following condition fails
    langs.distinct.size == langs.size
  }

  def languageIn(langs: List[String]): NodeChecker = attempt => node => for {
    t <- condition(checkLangIn(node,langs), attempt,
      languageInError(node, attempt, langs),
      s"$node satisfies languageIn(${langs.mkString(",")})")
  } yield t

  def checkLangIn(node: RDFNode, langs: List[String]): Boolean = {
    node match {
      case LangLiteral(_,l) => langs.contains(l.lang)
      case _ => false
    }
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

  def and(srefs: Seq[ShapeRef]): NodeChecker = attempt => node => {
    for {
      shapes <- getShapeRefs(srefs.toList,attempt,node)
      es = shapes.map(nodeShape(node, _))
      ts <- checkAll(es)
      t1 <- addEvidence(attempt, s"$node passes and(${shapes.map(_.showId).mkString(",")})")
      t <- combineTypings(t1 +: ts)
    } yield t
  }

  def xone(sRefs: Seq[ShapeRef]): NodeChecker = attempt => node => {
    for {
      shapes <- getShapeRefs(sRefs.toList, attempt, node)
      es = shapes.map(nodeShape(node, _))
      t1 <- checkOneOf(es,xoneErrorNone(node, attempt, sRefs.toList),
              xoneErrorMoreThanOne(node, attempt, sRefs.toList)
      )
      t2 <- addEvidence(attempt, s"$node passes xone(${shapes.map(_.showId).mkString(",")})")
      t <- combineTypings(Seq(t1, t2))
    } yield t
  }

  def qualifiedValueShape(shape: ShapeRef,
                          p: PropertyShape,
                          min: Option[Int],
                          max: Option[Int],
                          maybeDisjoint: Option[Boolean],
                          values: Seq[RDFNode],
                          attempt: Attempt,
                          path: SHACLPath): Check[ShapeTyping] = {
    val disjoint = maybeDisjoint.getOrElse(false)
    for {
      vs <- if (disjoint) filterConformSiblings(values, p, attempt)
            else ok(values)
      cs : List[Check[ShapeTyping]] = vs.toList.map(o => nodeShapeRef(o, shape, attempt))
      ts <- checkLs(cs)
      value = ts.length
      t <- condition(between(value, min, max),attempt,
          qualifiedShapeError(attempt.node, attempt, value, min, max),
          s"qualifiedValueShape value = ${value}, min=${min.map(_.toString).getOrElse("-")}, max=${max.map(_.toString).getOrElse("-")}"
        )
      ts1 <- combineTypings(t :: ts)
    } yield ts1
  }

  def filterConformSiblings(values: Seq[RDFNode], p: PropertyShape, attempt: Attempt): Check[Seq[RDFNode]] = {
    logger.info(s"FilterConformSiblings. values = $values, p = $p, attempt = $attempt")
    val shapes = schema.siblingQualifiedShapes(ShapeRef(p.id))
    logger.info(s"Sibling shapes: $shapes")
    for {
          rs <- filterConformShapes(values,shapes,attempt)
        }
     yield {
          logger.info(s"Result of filtering on filterConformSiblings($values,...) = $rs")
          if (rs.toList.length == values.toList.length) {
            logger.info("Size is equal...")
          } else {
            logger.info("******************** Size is different!!!")
          }
          rs
        }
  }

  def filterConformShapes(values: Seq[RDFNode], shapes: Seq[ShapeRef], attempt: Attempt): Check[Seq[RDFNode]] = {
    logger.info(s"FilterConformShapes(values=$values, shapes=$shapes)")
    def checkValuesShapes: Check[List[(RDFNode, Boolean)]] = {
      values.toList.map(value => conformsNodeShapes(value,shapes,attempt)).sequence
    }
    for {
      cs <- checkValuesShapes
      rs = cs.collect { case (n, false) => n }
    } yield {
      logger.info(s"Result of FilterConformShapes($values,$shapes,$attempt) = $rs")
      rs.toSeq
    }
  }

  def conformsNodeShapes(node: RDFNode, shapes: Seq[ShapeRef], attempt: Attempt): Check[(RDFNode,Boolean)] = for {
    ls <- checkLs(shapes.toList.map(nodeShapeRef(node,_,attempt)))
  } yield (node, !ls.isEmpty)

  def between(v: Int, maybeMin: Option[Int], maybeMax: Option[Int]): Boolean = (maybeMin, maybeMax) match {
    case (None, None) => true
    case (Some(min),None) => v >= min
    case (None,Some(max)) => v <= max
    case (Some(min),Some(max)) => v >= min && v <= max
  }

  def or(sRefs: Seq[ShapeRef]): NodeChecker = attempt => node => {
    val checks: List[CheckTyping] = sRefs.toList.map(s => {
      nodeShapeRef(node, s, attempt)
    })
    for {
      t0 <- getTyping
      t1 <- checkSome(checks, orError(node, attempt, sRefs.toList))
      t2 <- addEvidence(attempt, s"$node passes or(${sRefs.map(_.showId).mkString(",")})")
      t3 <- combineTypings(Seq(t1, t2))
    } yield t3
  }

  def not(sref: ShapeRef): NodeChecker = attempt => node => {
    val parentShape = attempt.nodeShape.shape
    val check: Shape => Check[ShapeTyping] = shape => nodeShape(node, shape)
    val handleError: Shape => ViolationError => Check[ShapeTyping] = s => e => for {
      t1 <- addNotEvidence(attempt, e,
        s"$node doesn't satisfy ${sref}. Negation declared in ${parentShape}. Error: $e")
      t2 <- addEvidence(attempt, s"$node satisfies not(${s.showId})")
      t <- combineTypings(List(t1, t2))
    } yield t
    val handleNotError: ShapeTyping => Check[ShapeTyping] = t =>
      err(notError(node, attempt, sref))
    for {
      shape <- getShapeRef(sref,attempt,node)
      t <- cond(check(shape), handleNotError, handleError(shape))
    } yield t
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

  def blankNodeOrLiteralChecker: NodeChecker = attempt => node => {
    condition(node.isBNode || node.isLiteral, attempt,
      bNodeOrLiteralKindError(node, attempt),
      s"$node is a Blank Node or Literal")
  }

  def iriOrLiteralChecker: NodeChecker = attempt => node => {
    condition(node.isIRI || node.isLiteral, attempt,
      iriOrLiteralKindError(node, attempt),
      s"$node is a IRI or Literal")
  }

  def hasValuePropertyChecker(v: Value, os: List[RDFNode], attempt: Attempt, node: RDFNode, path: SHACLPath): CheckTyping =
    os.size match {
     case 0 => err(hasValueErrorNoValue(node, attempt, v, path))
     case 1 => hasValue(v)(attempt)(os.head)
     case n => err(hasValueErrorMoreThanOne(node, attempt, v, path,n))
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

  def minCount(minCount: Int, os: Seq[RDFNode], attempt: Attempt, path: SHACLPath): Check[ShapeTyping] = {
    logger.info(s"minCount $minCount, os: $os, attempt: $attempt, path: $path")
    val count = os.size
    val node = attempt.node
    condition(count >= minCount, attempt,
      minCountError(node, attempt, minCount, os.size),
      s"Checked minCount($minCount) for path($path) on node $node")
  }

  def maxCount(maxCount: Int, os: Seq[RDFNode], attempt: Attempt, path: SHACLPath): Check[ShapeTyping] = {
    val count = os.size
    val node = attempt.node
    condition(count <= maxCount, attempt,
      maxCountError(node, attempt, maxCount, count),
      s"Checked maxCount($maxCount) for path($path) on node $node")
  }

  def checkClosed(ignoredProperties: List[IRI], allowedProperties: List[IRI]): NodeChecker = attempt => node => {
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

  def getShapeRefs(sRefs: List[ShapeRef], attempt: Attempt, node: RDFNode): Check[List[Shape]] =
      sRefs.map(getShapeRef(_,attempt,node)).sequence

  def getPropertyShapeRefs(srefs: List[ShapeRef], attempt: Attempt, node: RDFNode): Check[List[PropertyShape]] =
    srefs.map(getPropertyShapeRef(_,attempt,node)).sequence

  def getPropertyShapeRef(sref: ShapeRef, attempt: Attempt, node: RDFNode): Check[PropertyShape] = for {
    shape <- getShapeRef(sref,attempt,node)
    ps <- shape2PropertyShape(shape,attempt,node)
  } yield ps

  def shape2PropertyShape(shape: Shape, attempt: Attempt, node: RDFNode): Check[PropertyShape] = shape match {
    case ps: PropertyShape => ok(ps)
    case _ => err(expectedPropertyShape(node,attempt,s"Expected shape $shape to be a property shape"))
  }


  def getShapeRef(sref: ShapeRef, attempt: Attempt, node: RDFNode): Check[Shape] =
    schema.shapesMap.get(sref) match {
      case Some(shape) => ok(shape)
      case None => err(notFoundShapeRef(node, attempt,
        s"Shape ${sref.showId} not found in schema. Available srefs: ${schema.shapesMap.keys.map(_.showId).mkString(",")}")
      )
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
    newTyping <- addEvidence(attempt, evidence)
  } yield newTyping

  def addLogMsg(msg: String): Check[Unit] =
    addLog(List(MsgEvidence(msg)))

  def addEvidence(attempt: Attempt, msg: String): Check[ShapeTyping] = {
    val nodeShape = attempt.nodeShape
    for {
      t <- getTyping
      shape <- getShapeRef(nodeShape.shape,attempt,attempt.node)
      _ <- addLog(List(NodeShapeEvidence(nodeShape, msg)))
    } yield t.addEvidence(nodeShape.node, shape, msg)
  }

  def addNotEvidence(attempt: Attempt,
                     e: ViolationError,
                     msg: String): Check[ShapeTyping] = {
    val nodeShape = attempt.nodeShape
    val node = nodeShape.node
    val shape = nodeShape.shape
    for {
      t <- getTyping
      shape <- getShapeRef(attempt.nodeShape.shape, attempt,node)
      _ <- addLog(List(NodeShapeEvidence(nodeShape, msg)))
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

  // Fails if there is any error
  def validateAll(rdf: RDFReader): CheckResult[ViolationError, ShapeTyping, Log] = {
    implicit def showPair = new Show[(ShapeTyping, Evidences)] {
      def show(e: (ShapeTyping, Evidences)): String = {
        s"Typing: ${e._1}\n Evidences: ${e._2}"
      }
    }
    runCheck(checkSchemaAll, rdf)
  }

  def showResult(t: ShapeTyping): String = {
    def showRes(res: Either[Shape,Shape]) = res.fold("-" + _.showId,"+" + _.showId)
    t.simplified.map{
      case (node,res) => s"${node.toString} ${showRes(res)} "
    }.mkString(",")
  }
}

object Validator {
 def empty = Validator(schema = Schema.empty)

 type ShapeTyping = Typing[RDFNode,Shape,ViolationError,String]

 type Result[A] =  Either[NonEmptyList[ViolationError],List[(A,Evidences)]]

 def isOK[A](r: Result[A]): Boolean =
    r.isRight && r.toList.isEmpty == false


 def validate(schema: Schema, rdf: RDFReader): Either[ViolationError,ShapeTyping] = {
   Validator(schema).validateAll(rdf).result
 }


}

