package es.weso.shacl

import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.validating.{
  Constraint => VConstraint, 
  _
}
import ViolationError._
import Validated._

/**
 * This validator will be implemented directly in Scala
 */
case class CoreValidator(schema: Schema) {
  
  def validate: Seq[ValidationAttempt] = {
    scopeNodes.map {
      case (node,shape) => validateScopeNode(node,shape)
    }.flatten
  }
  
  def validateScopeNode(node: IRI, shape: Shape): Seq[ValidationAttempt] = {
    val results = validateConstraints(node,shape.components)
    Seq(ScopeNodeAttempt(node,shape,schema,results))
  }
  
  /**
   * Return all scopeNode declarations which are pairs (n,s) where
   * <p> `n` = node to validate
   * <p> `s` = candidate shape
   */
  def scopeNodes: Seq[(IRI,Shape)] = {
    schema.scopeNodeShapes
  }
  
  def validateConstraints(node: RDFNode, constraints: Seq[Constraint]): Seq[ViolationError] = {
    val zero: Seq[ViolationError] = Seq()
    def combine(errors: Seq[ViolationError], constraint: Constraint): Seq[ViolationError] = {
      checkConstraint(node,constraint) ++ errors
    }
    constraints.foldLeft(zero)(combine)
  }
  
  
  def checkConstraint(node: RDFNode, constraint: Constraint): Seq[ViolationError] = {
    constraint match {
//      case pc: PropertyConstraint => checkPropertyConstraint(node,pc)
      case _ => Seq(ViolationError.unsupportedError(node))
    }
  }

  def shapeConstraint: ShapeConstraint = Single((shape,rdf) => {
    val nodes = shape.scopeNodes
    val results = nodes.map(n => shapeNodeConstraint.validate((n,shape),rdf))
//    val result = Validated.all(results)
//    result.mapValue(_ => shape)
    ???
  })
  
  def shapeNodeConstraint: NodeShapeConstraint = Single((pair,rdf) => {
    val (node,shape) = pair
    val cs = shape.components.map(vConstraint)
    val c = All(cs) 
    val result = c.validate(node,rdf) 
    result.mapValue(n => (n,shape)) 
  })
  
  def vConstraint(c:Constraint): RDFNodeConstraint = {
    c match {
      case pc:PropertyConstraint => vPropertyConstraint(pc)
      case _ => ???
    }
  }
  
  def vPropertyConstraint(pc: PropertyConstraint): 
       RDFNodeConstraint = Single((node,rdf) => {
    val predicate = pc.predicate
    val components = pc.components
    val pcConstraints = components.map(component2VPropertyConstraint(predicate))
    val result : VPropertyConstraint = All(pcConstraints)
    result.validate(node, (rdf,predicate))
  })
  
  def component2VPropertyConstraint(p:IRI)(c: PCComponent): VPropertyConstraint = {
    c match {
      case MinCount(n) => minCount(n)
      case MaxCount(n) => maxCount(n)
      case _ => unsupported(c)
    }
  }

  type ShapeConstraint = VConstraint[RDFReader,Shape,Throwable]
  type NodeShapeConstraint = VConstraint[RDFReader,(RDFNode,Shape),Throwable]
  type RDFNodeConstraint = VConstraint[RDFReader, RDFNode, Throwable]
  type VPropertyConstraint = VConstraint[(RDFReader,IRI), RDFNode, Throwable]
  
  type Explain = String
  def explain(message: String) = message 
  
  def minCount(minCount: Int): VPropertyConstraint = Single((node,ctx) => {
    val (rdf,predicate) = ctx
    val count = rdf.triplesWithSubjectPredicate(node,predicate).size
    if (count < minCount) {
      err(minCountError(node,predicate,minCount,count))
    }
    else ok(SingleReason(node,
      s"$node satisfies minCount=$minCount for predicate $predicate with count=$count"))
   })
   
  def maxCount(maxCount: Int): VPropertyConstraint = Single((node,ctx) => {
    val (rdf,predicate) = ctx
    val count = rdf.triplesWithSubjectPredicate(node,predicate).size
    if (count > maxCount) {
      err(maxCountError(node,predicate,maxCount,count))
    }
    else ok(SingleReason(node,
      explain(s"$node satisfies maxCount=$maxCount for predicate $predicate with count=$count")))
   })
   
   
  def unsupported(c: PCComponent): VPropertyConstraint = Single((node,ctx) => {
      err(Unsupported(s"Property constraint $c not implemented yet"))  
   })
   
}

object CoreValidator {
  def empty = CoreValidator(schema = Schema.empty)
}

