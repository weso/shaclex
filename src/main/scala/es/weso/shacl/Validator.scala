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
 
 /**
  * Checks that a node satisfies a shape 
  */
 type PropertyChecker = (NodeShapeEntry,IRI) => Check[RDFNode]
 type NodeChecker = (NodeShapeEntry, RDFNode) => Check[RDFNode]
  
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
    val r = checkAll(cs.map(c => c((node,shape),node)))
    r.map(_ => nodeShape)
  } 
  
  def checkConstraint(c:Constraint): NodeChecker = {
    c match {
      case pc:PropertyConstraint => checkPropertyConstraint(pc)
    }
  }
  
  def checkPropertyConstraint(pc: PropertyConstraint): NodeChecker = (nodeShape,_) => {
    val predicate = pc.predicate
    val components = pc.components
    val (node,shape) = nodeShape
    val pcConstraints: Seq[PropertyChecker] = components.map(component2PropertyChecker)
    validateCheckers(node, shape, predicate, pcConstraints)
  }
  
  def validateCheckers(n: RDFNode, shape: Shape, p: IRI, cs: Seq[PropertyChecker]): Check[RDFNode] = {
    val xs : Seq[Check[RDFNode]] = cs.map(c => c((n,shape),p))
    val r: Check[Seq[RDFNode]] = checkAll(xs)
    r.map(_ => n)
  }

  
  def component2PropertyChecker(c: PCComponent): PropertyChecker = 
    (nodeShape,predicate) => for {
     rdf <- getRDF
     val (node,shape) = nodeShape
     val os = rdf.triplesWithSubjectPredicate(node,predicate).map(_.obj).toSeq
     val check: Check[Seq[RDFNode]] = c match {
      case MinCount(n) => minCount(n, os, nodeShape, predicate)
      case MaxCount(n) => maxCount(n, os, nodeShape, predicate)
      case In(values) => checkSeq(os, (o: RDFNode) => inChecker(values)(nodeShape,o))
//      case k: NodeKindType => checkSeq(os, (o: RDFNode) => nodeKindChecker(k)(nodeShape,o))
     }
     _ <- check
    } yield node  
   
/*  def nodeKindChecker(k: NodeKindType): NodeChecker = (nodeShape,currentNode) => {
    val (node,shape) = nodeShape
    for {
      _ <- conditionNode(
             inValues(currentNode,values), nodeShape, 
             inError(currentNode,values), 
             s"Checked $currentNode sh:in $values")
    } yield node
  } */
  
  def conditionSet(cond: RDFNode => Boolean,nodes: Set[RDFNode], shape: Shape, error: ViolationError, evidence: String): Check[Unit] = {
    val zero: Check[Unit] = pure(())
    def next(current: Check[Unit], n: RDFNode): Check[Unit] = for {
      _ <- current
      _ <- condition(cond(n),(n,shape),error,evidence)
    } yield ()
    nodes.foldLeft(zero)(next)
  }

  def conditionNode(
      condition: Boolean,
      nodeShape: NodeShapeEntry,
      error: ViolationError,
      evidence: String): Check[Unit] = for {
        _ <- validateCheck[Comput,ViolationError](condition,error)
        _ <- addEvidence(nodeShape, evidence)
      } yield ()

  def condition(
      condition: Boolean,
      nodeShape: NodeShapeEntry,
      error: ViolationError,
      evidence: String): Check[Unit] = for {
        _ <- validateCheck[Comput,ViolationError](condition,error)
        _ <- addEvidence(nodeShape, evidence)
      } yield ()

  def inValues(node:RDFNode, values: Seq[Value]): Boolean = {
   values.exists(_.matchNode(node))
  }
      
  def inChecker(values: Seq[Value]): NodeChecker = (nodeShape,currentNode) => {
    val (node,shape) = nodeShape
    for {
     _ <- conditionNode(
             inValues(currentNode,values), nodeShape, 
             inError(currentNode,values), 
             s"Checked $currentNode sh:in $values")
    } yield node
  }
  
  def getRDF: Check[RDFReader] = ask[Comput,RDFReader]

  def minCount(minCount: Int, os: Seq[RDFNode], nodeShape: NodeShapeEntry, predicate: IRI): Check[Seq[RDFNode]] = {
    val count = os.size
    val (node,shape) = nodeShape
    for {
     _ <- condition(count >= minCount, nodeShape, 
            minCountError(node,predicate,minCount,os.size),
            s"Checked minCount($minCount) for predicate($predicate) on node $node")
   } yield os
  }

  def maxCount(maxCount: Int, os: Seq[RDFNode], nodeShape: NodeShapeEntry, predicate: IRI): Check[Seq[RDFNode]] = {
    val count = os.size
    val (node,shape) = nodeShape
    for {
     _ <- condition(count <= maxCount, nodeShape, 
          maxCountError(node,predicate,maxCount,count),
          s"Checked maxCount($maxCount) for predicate($predicate) on node $node")
    } yield os
  }
   
  def addEvidence(nodeShape: NodeShapeEntry, msg: String): Check[Unit] = for {
    typing <- get[Comput,Typing]
    // TODO: Check that (node, shape) are right
    _ <- put[Comput,Typing](typing.addAction(nodeShape,msg)) 
  } yield ()

   // Generic definitions...
   
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
  
 def runCheck[A](c: Check[A], rdf: RDFReader): Result[A] = 
   c.runReader(rdf).runState(Typing.empty).runChoose.runNel.runEval.run

   
}

