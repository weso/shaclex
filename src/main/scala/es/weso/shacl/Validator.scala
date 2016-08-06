package es.weso.shacl
import es.weso.rdf._
import es.weso.rdf.nodes._
import ViolationError._
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._

/**
 * This validator is implemented directly in Scala
 */
case class Validator(schema: Schema) {

  
   
 type Checker[A] = A => Check[A]
 type ShapeChecker = Checker[Shape]
 type NodeShapeChecker = Checker[(RDFNode,Shape)]
 type PropertyChecker = (RDFNode,Shape,IRI) => Check[RDFNode]
 type NodeChecker = (RDFNode,Shape) => Check[RDFNode]
  
  /**
   * Return all scopeNode declarations which are pairs (n,s) where
   * <p> `n` = node to validate
   * <p> `s` = candidate shape
   */
  def targetNodes: Seq[(IRI,Shape)] = {
    schema.targetNodeShapes
  }
 
  // Fails if there is any error
  def validateAll(rdf: RDFReader): CheckResult = {
   val r: Xor[NonEmptyList[ViolationError],List[(Schema,Typing)]] = 
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
  
  def nodeShape: NodeShapeChecker = nodeShape => {
    val (node,shape) = nodeShape
    val cs = shape.components.map(checkConstraint)
    val r = checkAll(cs.map(c => c(node,shape)))
    r.map(_ => nodeShape)
  } 
  
  def checkConstraint(c:Constraint): NodeChecker = {
    c match {
      case pc:PropertyConstraint => checkPropertyConstraint(pc)
    }
  }
  
  def checkPropertyConstraint(pc: PropertyConstraint): NodeChecker = (node,shape) => {
    val predicate = pc.predicate
    val components = pc.components
    val pcConstraints: Seq[PropertyChecker] = components.map(component2PropertyChecker)
    validateCheckers(node, shape, predicate, pcConstraints)
  }
  
  def validateCheckers(n: RDFNode, shape: Shape, p: IRI, cs: Seq[PropertyChecker]): Check[RDFNode] = {
    val xs : Seq[Check[RDFNode]] = cs.map(c => c(n,shape,p))
    val r: Check[Seq[RDFNode]] = checkAll(xs)
    r.map(_ => n)
  }

  // TODO
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
  
  
  def component2PropertyChecker(c: PCComponent): PropertyChecker = {
    c match {
      case MinCount(n) => minCount(n)
      case MaxCount(n) => maxCount(n)
      case In(values) => inPropertyChecker(values)
    }
  }

  def conditionSet(cond: RDFNode => Boolean,nodes: Set[RDFNode], shape: Shape, error: ViolationError, evidence: String): Check[Unit] = {
    val zero: Check[Unit] = pure(())
    def next(current: Check[Unit], n: RDFNode): Check[Unit] = for {
      _ <- current
      _ <- condition(cond(n),n,shape,error,evidence)
    } yield ()
    nodes.foldLeft(zero)(next)
  }

  def condition(
      condition: Boolean,
      node: RDFNode,
      shape: Shape,
      error: ViolationError,
      evidence: String): Check[Unit] = for {
        _ <- validateCheck[Comput,ViolationError](condition,error)
        _ <- addEvidence(node, shape, evidence)
      } yield ()

  def inValues(node:RDFNode, values: Seq[Value]): Boolean = {
   values.exists(_.matchNode(node))
  }
      
  def inPropertyChecker(values: Seq[Value]): PropertyChecker = (node,shape,predicate) => for {
    rdf <- ask[Comput,RDFReader]
    val os = rdf.triplesWithSubjectPredicate(node,predicate).map(_.obj)
    _ <- conditionSet(n => inValues(n,values),os, shape, inError(node,values), s"Checked $node sh:in $values") 
  } yield node

  def minCount(minCount: Int): PropertyChecker = (node,shape,predicate) => for {
     rdf <- ask[Comput,RDFReader]
     val count = rdf.triplesWithSubjectPredicate(node,predicate).size
     _ <- condition(count >= minCount, node, shape, 
            minCountError(node,predicate,minCount,count),
            s"Checked minCount($minCount) for predicate($predicate) on node $node")
   } yield node 

   def maxCount(maxCount: Int): PropertyChecker = (node,shape,predicate) => for {
     rdf <- ask[Comput,RDFReader]
     val count = rdf.triplesWithSubjectPredicate(node,predicate).size
     _ <- condition(count <= maxCount, node, shape, 
           maxCountError(node,predicate,maxCount,count),
           s"Checked maxCount($maxCount) for predicate($predicate) on node $node")
   } yield node
   
   def addEvidence(node: RDFNode, shape: Shape, msg: String): Check[Unit] = for {
     typing <- get[Comput,Typing]
     // TODO: Check that (node, shape) are right
     _ <- put[Comput,Typing](typing.addAction(node,shape,msg)) 
   } yield ()
}

object Validator {
  
 // With this import we use list for non determinism
 // We could import Option if we are only interested in one answer
 import cats.std.list._

 def empty = Validator(schema = Schema.empty)
  
 def runCheck[A](c: Check[A], rdf: RDFReader): Result[A] = 
   c.runReader(rdf).runState(Typing.empty).runChoose.runNel.runEval.run
  
}

