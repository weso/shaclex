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

  type Comput =  
    Reader[RDFReader,?] |:
    State[Typing,?] |:
    Choose |: 
    Validate[ViolationError, ?] |:
    Eval |:
    NoEffect

  type Result[A] =  Xor[NonEmptyList[ViolationError],List[(A,Actions)]]
  
  def isOK[A](r: Result[A]): Boolean = 
    r.isRight && r.toList.isEmpty == false  

 type Check[A] = Eff[Comput,A]
 type Checker[A] = A => Check[A]
 type ShapeChecker = Checker[Shape]
 type NodeShapeChecker = Checker[(RDFNode,Shape)]
 type PropertyChecker = (RDFNode,Shape,IRI) => Check[RDFNode]
 type NodeChecker = (RDFNode,Shape) => Check[RDFNode]
  
 // With this import we use list for non determinism
 // We could import Option if we are only interested in one answer
 import cats.std.list._

  
  /**
   * Return all scopeNode declarations which are pairs (n,s) where
   * <p> `n` = node to validate
   * <p> `s` = candidate shape
   */
  def scopeNodes: Seq[(IRI,Shape)] = {
    schema.scopeNodeShapes
  }
 
  // Fails if there is any error
  def validateAll(rdf: RDFReader): CheckResult = {
   val r: Xor[NonEmptyList[ViolationError],List[(Schema,Typing)]] = 
     checkSchemaAll(schema).runReader(rdf).runState(Typing.empty).runChoose.runNel.runEval.run
   CheckResult(r)
  }
  
  /**
   * Checks if all nodes/shapes are valid in a schema
   * Fails if any of them is not correct 
   */
  def checkSchemaAll: Checker[Schema] = schema => {
    val shapes = schema.shapes
    val results: Seq[Check[Shape]] = shapes.map(sh => shapeConstraint(sh))
    val r = checkAll(results)
    r.map(_ => schema)
  }

  def checkSchemaSome: Checker[Schema] = schema => {
    val shapes = schema.shapes
    val results: Seq[Check[Shape]] = shapes.map(sh => shapeConstraint(sh))
    val r = checkAll(results)
    r.map(_ => schema)
  }
  
  def shapeConstraint: ShapeChecker = shape => {
    val nodes = shape.scopeNodes
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
  def checkSome[A](xs: Seq[Check[A]]): Check[Seq[A]] = {
    val zero: Check[Seq[A]] = pure(Seq())
    def next(x: Check[A], rest: Check[Seq[A]]): Check[Seq[A]] = {
      ???
    }
/*      *for {
      v <- x
      rs <- rest
    } yield v +: rs */
    xs.foldRight(zero)(next)
  }

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
    }
  }

   def minCount(minCount: Int): PropertyChecker = (node,shape,predicate) => for {
     rdf <- ask[Comput,RDFReader]
     val count = rdf.triplesWithSubjectPredicate(node,predicate).size
     _ <- validateCheck[Comput,ViolationError](count >= minCount, minCountError(node,predicate,minCount,count))
     _ <- addEvidence(node,shape,s"Checked minCount($minCount) for predicate($predicate) on node $node")
   } yield node 

   def maxCount(maxCount: Int): PropertyChecker = (node,shape,predicate) => for {
     rdf <- ask[Comput,RDFReader]
     val count = rdf.triplesWithSubjectPredicate(node,predicate).size
     _ <- validateCheck[Comput,ViolationError](count <= maxCount, maxCountError(node,predicate,maxCount,count))
     _ <- addEvidence(node,shape, s"Checked maxCount($maxCount) for predicate($predicate) on node $node")
   } yield node
   
   def addEvidence(node: RDFNode, shape: Shape, msg: String): Check[Unit] = for {
     typing <- get[Comput,Typing]
     // TODO: Check that (node, shape) are right
     _ <- put[Comput,Typing](typing.addAction(node,shape,msg)) 
   } yield ()
}

object EffValidator {
  def empty = Validator(schema = Schema.empty)
  
}

