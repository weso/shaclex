package es.weso.shacl

import es.weso.rdf._
import es.weso.rdf.nodes._

/**
 * This validator will be implemented directly in Scala
 */
case class CoreValidator(rdf: RDFReader, schema: Schema) {
  
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
      case pc: PropertyConstraint => checkPropertyConstraint(node,pc)
      case _ => Seq(ViolationError.unsupportedError(node))
    }
  }
  
  def checkPropertyConstraint(node: RDFNode, pc: PropertyConstraint): Seq[ViolationError] = {
    val predicate = pc.predicate
    // TODO
    ???
  }
}

