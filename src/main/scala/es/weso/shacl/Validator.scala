package es.weso.shacl
import es.weso.rdf._
import es.weso.rdf.nodes._
import ViolationError._
import cats._, data._
import cats.syntax.all._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import util.matching._

/**
 * This validator is implemented directly in Scala
 */
case class Validator(schema: Schema) {

 type Checker[A] = A => Check[A]
 type ShapeChecker = Checker[Shape]
 type NodeShapeChecker = Checker[(RDFNode,Shape)]
  
 /**
  * Checks that a node satisfies a shape 
  */
 type PropertyChecker = (Attempt,IRI) => Check[RDFNode]
 type NodeChecker = (Attempt, RDFNode) => Check[RDFNode]
  
  /**
   * Return all scopeNode declarations which are pairs (n,s) where
   * <p> `n` = node to validate
   * <p> `s` = candidate shape
   */
  def targetNodes: Seq[(RDFNode,Shape)] = {
    schema.targetNodeShapes
  }
 
  // Fails if there is any error
  def validateAll(rdf: RDFReader): CheckResult = {
   val r: Xor[NonEmptyList[ViolationError],List[(Schema,ShapeTyping)]] = 
     Validator.runCheck(checkSchemaAll(schema),rdf)
   CheckResult(r)
  }
  
  /**
   * Checks if all nodes/shapes are valid in a schema
   * Fails if any of them is not correct 
   */
  def checkSchemaAll: Checker[Schema] = schema => {
    val shapes = schema.shapes
    val results: Seq[Check[Shape]] = shapes.map(sh => shapeChecker(sh))
    val r = checkAll(results)
    r.map(_ => schema)
  }

  def checkSchemaSome: Checker[Schema] = schema => {
    val shapes = schema.shapes
    val results: Seq[Check[Shape]] = shapes.map(sh => shapeChecker(sh))
    val r = checkAll(results)
    r.map(_ => schema)
  }
  
  def shapeChecker: ShapeChecker = shape => {
    val nodes = shape.targetNodes
    val nodesShapes: Seq[Check[(RDFNode,Shape)]] = nodes.map(n => nodeShape(n,shape))
    val r = checkAll(nodesShapes)
    r.map(_ => shape)
  } 
  
  def nodeShape: NodeShapeChecker = { case (node,shape) => {
    println(s"Checking if $node matches $shape") 
    val cs = shape.constraints.map(checkConstraint)
    val r = checkAll(cs.map(c => c(Attempt(NodeShape(node,shape), None),node)))
    r.map(_ => (node,shape))
   } 
  }
  
  def checkConstraint(c:Constraint): NodeChecker = {
    c match {
      case pc:PropertyConstraint => checkPropertyConstraint(pc)
      case pc:PathPropertyConstraint => throw new Exception(s"Non-implemented PathProperty Constraints yet: $pc")
      case nc:NodeConstraint => checkNodeConstraint(nc)
    }
  }
  
  def checkNodeConstraint(nc: NodeConstraint): NodeChecker = (attempt,node) => {
    val components = nc.components
    val checkers: Seq[NodeChecker] = components.map(component2Checker)
    validateNodeCheckers(attempt,checkers)
  }
  
  def checkPropertyConstraint(pc: PropertyConstraint): NodeChecker = (attempt,_) => {
    val predicate = pc.predicate
    val components = pc.components
    val propertyCheckers: Seq[PropertyChecker] = components.map(component2PropertyChecker)
    validatePredicateCheckers(attempt, predicate, propertyCheckers)
  }
  
  def validateNodeCheckers(attempt: Attempt, cs: Seq[NodeChecker]): Check[RDFNode] = {
    val newAttempt = attempt.copy(path = None)
    val xs : Seq[Check[RDFNode]] = cs.map(c => c(newAttempt,newAttempt.node))
    val r: Check[Seq[RDFNode]] = checkAll(xs)
    r.map(_ => newAttempt.node)
  }
  
  def validatePredicateCheckers(attempt: Attempt, p: IRI, cs: Seq[PropertyChecker]): Check[RDFNode] = {
    val newAttempt = attempt.copy(path = Some(p))
    val xs : Seq[Check[RDFNode]] = cs.map(c => c(newAttempt,p))
    val r: Check[Seq[RDFNode]] = checkAll(xs)
    r.map(_ => newAttempt.node)
  }

  def component2Checker(c: Component): NodeChecker =
    (attempt,node) => {
      c match {
        case NodeKind(k) => nodeKindChecker(k)(attempt,node)
        case And(shapes) => and(shapes)(attempt,node)
        case _ => unsupportedNodeChecker(s"$c")(attempt,node)
      }
    }
  
  def component2PropertyChecker(c: Component): PropertyChecker = 
    (attempt,predicate) => for {
     rdf <- getRDF
     val node = attempt.node
     val os = rdf.triplesWithSubjectPredicate(node,predicate).map(_.obj).toSeq
     val check: Check[Seq[RDFNode]] = c match {
      case ShapeComponent(s) => checkSeq(os, shapeComponentChecker(s)(attempt,_))
      case ClassComponent(c) => checkSeq(os, classComponentChecker(c)(attempt,_))
      case Datatype(d) => checkSeq(os, datatypeChecker(d)(attempt,_))
      case NodeKind(k) => checkSeq(os, nodeKindChecker(k)(attempt,_))
      case MinCount(n) => minCount(n, os, attempt, predicate)
      case MaxCount(n) => maxCount(n, os, attempt, predicate)
      case MinExclusive(n) => checkSeq(os, minExclusive(n)(attempt,_))
      case MinInclusive(n) => checkSeq(os, minInclusive(n)(attempt,_))
      case MaxInclusive(n) => checkSeq(os, maxInclusive(n)(attempt,_))
      case MaxExclusive(n) => checkSeq(os, maxExclusive(n)(attempt,_))
      case MinLength(n) => checkSeq(os, minLength(n)(attempt,_))
      case MaxLength(n) => checkSeq(os, maxLength(n)(attempt,_))
      case Pattern(p,flags) => checkSeq(os, pattern(p,flags)(attempt,_))
      case UniqueLang(v) => checkSeq(os, unsupportedNodeChecker("UniqueLang")(attempt,_))
      case And(shapes) => checkSeq(os, and(shapes)(attempt,_))
      case Or(shapes) => checkSeq(os, or(shapes)(attempt,_))
      case Not(shape) => checkSeq(os, not(shape)(attempt,_))
      case HasValue(v) => checkSeq(os, unsupportedNodeChecker("HasValue")(attempt,_))      
      case In(values) => checkSeq(os, inChecker(values)(attempt,_))
      case _ => throw new Exception(s"Unsupported check $c")
     }
     _ <- check
    } yield node  

  def shapeComponentChecker(s: Shape): NodeChecker = (attempt,node) => for {
    typing <- getTyping
    _ <- if (typing.getOkValues(node).contains(s)) pure[Comput,(RDFNode,Shape)]((node,s))
         else if (typing.getFailedValues(node).contains(s)) {
           wrong[Comput,ViolationError](failedNodeShape(
               node,s,attempt,
               s"Failed because $node doesn't match shape $s")) >>
           pure((node,s))
         } else nodeShape(node,s)
    _ <- addEvidence(attempt.nodeShape, s"$node has shape ${s.id.getOrElse("?")}") 
  } yield node

  def classComponentChecker(cls: RDFNode): NodeChecker = (attempt,node) => for {
    rdf <- getRDF
    _ <- condition(rdf.hasSHACLClass(node,cls),
        attempt,
        classError(node,cls,attempt),
        s"$node is in class $cls")
  } yield node
        
  def nodeKindChecker(k: NodeKindType): NodeChecker = (attempt,node) => 
  k match {
    case IRIKind => iriChecker(attempt,node)
    case LiteralKind => literalChecker(attempt,node)
    case BlankNodeKind => blankNodeChecker(attempt,node)
    case BlankNodeOrIRI => blankNodeOrIRIChecker(attempt,node)
    case BlankNodeOrLiteral => blankNodeOrLiteralChecker(attempt,node)
    case IRIOrLiteral => iriOrLiteralChecker(attempt,node)
  }

  def datatypeChecker(d: IRI): NodeChecker = (attempt,node) =>
    condition(hasDatatype(node,d),
        attempt,
        datatypeError(node,d,attempt),
        s"$node has datatype $d")
        
  def unsupportedNodeChecker(msg:String): NodeChecker = (attempt,node) => {
    wrong[Comput,ViolationError](unsupported(node,attempt,msg)) >>
    pure(node)
  }

  def iriChecker: NodeChecker = (attempt,node) => {
    condition(node.isIRI, attempt, 
        iriKindError(node,attempt),
        s"$node is an IRI") 
  }

  def compareIntLiterals(
      n:Literal, 
      f: (Int, Int) => Boolean, 
      err: (RDFNode, Attempt, Int) => ViolationError, 
      msg: String): NodeChecker = (attempt,node) => for {
    ctrolValue <- checkNumeric(n, attempt)
    value <- checkNumeric(node, attempt)
    _ <- condition(f(ctrolValue,value), attempt, 
        err(node, attempt, ctrolValue),
        s"$node satisfies $msg($n)") 
  } yield node

  
  def minExclusive(n:Literal): NodeChecker = 
    compareIntLiterals(n,_ < _, minExclusiveError, "minExclusive")
    
  def minInclusive(n:Literal): NodeChecker = 
    compareIntLiterals(n,_ <= _, minInclusiveError, "minInclusive")
    
  def maxExclusive(n:Literal): NodeChecker = 
    compareIntLiterals(n,_ > _, maxExclusiveError, "maxExclusive")
    
  def maxInclusive(n:Literal): NodeChecker = 
    compareIntLiterals(n,_ >= _, maxInclusiveError, "maxInclusive")
    
  def minLength(n:Int): NodeChecker = (attempt,node) => for {
    _ <- condition(node.getLexicalForm.length >= n, attempt, 
        minLengthError(node,attempt,n),
        s"$node satisfies minLength($n)") 
  } yield node
  
  def maxLength(n:Int): NodeChecker = (attempt,node) => for {
    _ <- condition(node.getLexicalForm.length <= n, attempt, 
         maxLengthError(node,attempt,n),
         s"$node satisfies maxLength($n)")
  } yield node
    
  def pattern(p:String, flags: Option[String]): NodeChecker = (attempt,node) => for {
    pattern <- getRegex(p,flags)
    _ <- condition(regexMatch(pattern,node), attempt, 
         patternError(node,attempt,p,flags),
         s"$node satisfies pattern '$p'${flags.getOrElse("")})")
  } yield node
  
  // TODO: Use xerces implementation of XPath regex instead of Scala's builtin
  def getRegex(p: String, flags: Option[String]): Check[Regex] = {
    pure(new Regex(p))
  }
  
  def regexMatch(pattern: Regex, n: RDFNode): Boolean = {
    pattern.findFirstIn(n.getLexicalForm).isDefined
  }
  
  def and(shapes:Seq[Shape]): NodeChecker = (attempt,node) => {
    import cats.std.list._
    val es = shapes.map(s => nodeShape(node,s))
    println(s"Shapes in and: $shapes") 
    checkAll(es).map(_ => node)
  }
  
  def or(shapes:Seq[Shape]): NodeChecker = (attempt,node) => 
    throw new Exception(s"Non implemented sh:or yet. Shapes: $shapes, node: $node, attempt: $attempt")
  /*{
    _ <- chooseFrom
  } yield node */
  
  def not(shape: Shape): NodeChecker = (attempt,node) => for {
    _ <- nodeShape(node,shape)
  } yield node
  
  
  
  def checkNumeric(node: RDFNode, attempt: Attempt): Check[Int] = 
    node match {
    case n:IntegerLiteral => pure(n.int)
    case n:DecimalLiteral => pure(n.decimal.toInt)
    case n:DoubleLiteral => pure(n.double.toInt)
    case _ => wrong[Comput,ViolationError](notNumeric(node, attempt)) >> pure(0)
  }

  def literalChecker: NodeChecker = (attempt,node) => {
    condition(node.isLiteral, attempt, 
        literalKindError(node, attempt),
        s"$node is a Literal") 
  }

  def blankNodeChecker: NodeChecker = (nodeShape,node) => {
    condition(node.isBNode, nodeShape, 
        bNodeKindError(node, nodeShape),
        s"$node is a Blank Node") 
  }
  
  def blankNodeOrIRIChecker: NodeChecker = (nodeShape,node) => {
    condition(node.isBNode || node.isIRI, nodeShape, 
        bNodeOrIRIKindError(node, nodeShape),
        s"$node is a Blank Node or an IRI") 
  }
  
  def blankNodeOrLiteralChecker: NodeChecker = (nodeShape,node) => {
    condition(node.isBNode || node.isLiteral, nodeShape, 
        bNodeOrLiteralKindError(node, nodeShape),
        s"$node is a Blank Node or Literal") 
  }

  def iriOrLiteralChecker: NodeChecker = (attempt,node) => {
    condition(node.isIRI || node.isLiteral, attempt, 
        iriOrLiteralKindError(node, attempt),
        s"$node is a IRI or Literal") 
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
      evidence: String): Check[RDFNode] = for {
        _ <- validateCheck[Comput,ViolationError](condition,error)
        _ <- addEvidence(attempt.nodeShape, evidence)
      } yield attempt.node

      
  def inChecker(values: Seq[Value]): NodeChecker = (attempt,currentNode) => {
    condition(inValues(currentNode,values), attempt, 
              inError(currentNode,attempt, values), 
              s"Checked $currentNode sh:in $values") >> 
    pure(attempt.node)
  }
  
  def getRDF: Check[RDFReader] = ask[Comput,RDFReader]
  
  def getTyping: Check[ShapeTyping] = get[Comput,ShapeTyping]

  def minCount(minCount: Int, os: Seq[RDFNode], attempt: Attempt, predicate: IRI): Check[Seq[RDFNode]] = {
    val count = os.size
    val node = attempt.node
    for {
     _ <- condition(count >= minCount, attempt, 
            minCountError(node, attempt, minCount,os.size),
            s"Checked minCount($minCount) for predicate($predicate) on node $node")
   } yield os
  }

  def maxCount(maxCount: Int, os: Seq[RDFNode], attempt: Attempt, predicate: IRI): Check[Seq[RDFNode]] = {
    val count = os.size
    val node = attempt.node
    for {
     _ <- condition(count <= maxCount, attempt, 
          maxCountError(node,attempt, maxCount,count),
          s"Checked maxCount($maxCount) for predicate($predicate) on node $node")
    } yield os
  }
   
  def addEvidence(nodeShape: NodeShape, msg: String): Check[Unit] = {
   println(s"Adding evidence $nodeShape, $msg")
   for {
    typing <- get[Comput,ShapeTyping]
    // TODO: Check that (node, shape) are right
    _ <- put[Comput,ShapeTyping](typing.addEvidence(nodeShape.node, nodeShape.shape,msg)) 
  } yield ()
  }

  ////////////////////////////////////////////
  /**
   * Checks that `node` is one of `values` 
   */
  def inValues(node:RDFNode, values: Seq[Value]): Boolean = {
   values.exists(_.matchNode(node))
  }
  
  def hasDatatype(node: RDFNode, d: IRI): Boolean = {
    node match {
      case l: Literal => l.dataType == d
      case _ => false
    }
  }

  
  //////////////////////////////
  // Generic definitions related to effects...could be moved to some utils file...
   
  // TODO: This definition could probably be generalized using traverse's sequence
  def checkSeq[A](s: Seq[A], check: A => Check[A]): Check[Seq[A]] = {
    val zero: Check[Seq[A]] = pure(Seq())
    def next(current: Check[Seq[A]], x: A): Check[Seq[A]] = for {
      r <- check(x)
      xs <- current
    } yield (r +: xs)
    s.foldLeft(zero)(next) 
  }

  // TODO
  // Define a more general method?
  def checkAny(xs: Seq[Check[(RDFNode,Shape)]]): Check[Seq[(RDFNode,Shape)]] = {
    val zero: Check[Seq[(RDFNode,Shape)]] = pure(Seq())
    def next(
        x: Check[(RDFNode,Shape)], 
        rest: Check[Seq[(RDFNode,Shape)]]): Check[Seq[(RDFNode,Shape)]] = ??? 
/*      for {
       rs1 <- catchWrong(x.flatMap(v => Seq(x)))(_ => pure(Seq()))
       rs2 <- rest
      } rs1 ++ rs2 */
    xs.foldRight(zero)(next)
  }

  /**
   * Checks all the values in a sequence
   */
  def checkAll[A](xs: Seq[Check[A]]): Check[Seq[A]] = {
    val zero: Check[Seq[A]] = pure(Seq())
    def next(x: Check[A], rest: Check[Seq[A]]): Check[Seq[A]] = for {
      v <- x
      rs <- rest
    } yield v +: rs
    xs.foldRight(zero)(next)
  }
  
}

object Validator {
  
 // With this import we use list for non determinism
 // We could import Option if we are only interested in one answer
 import cats.std.list._

 def empty = Validator(schema = Schema.empty)
  
 def runCheck[A](c: Check[A], rdf: RDFReader): Result[A] = {
   val initial : ShapeTyping = Typing.empty
   c.runReader(rdf).runState(initial).runChoose.runNel.runEval.run
 }
   
}

