package es.weso.shacl

import es.weso.rdf.nodes._
import SHACLPrefixes._
import es.weso.validating._

case class ViolationError(
    id: IRI,
    focusNode: RDFNode,
    subject: Option[RDFNode],
    predicate: Option[RDFNode],
    obj: Option[RDFNode],
    message: Option[String],
    sourceConstraint: Option[RDFNode]
) extends ConstraintError("Violation error")

object ViolationError {
  
  def unsupported(focusNode: RDFNode, msg: String) =  
    ViolationError(
      id = sh + "unsupported",
      focusNode = focusNode,
      subject = None,
      predicate = None,
      obj = None,
      message = Some("Unsupported: " + msg),
      sourceConstraint = None)
      
  def minCountError(focusNode: RDFNode, predicate: IRI, minCount: Int, count: Int) = 
    ViolationError(id = sh + "minCountError",
        focusNode = focusNode,
        subject = Some(focusNode),
        predicate = Some(predicate),
        obj = None,
        message = Some(s"MinCount violation. Expected $minCount, obtained: $count"),
        sourceConstraint = None)
        
  def maxCountError(focusNode: RDFNode, predicate: IRI, maxCount: Int, count: Int) = 
    ViolationError(id = sh + "maxCountError",
        focusNode = focusNode,
        subject = Some(focusNode),
        predicate = Some(predicate),
        obj = None,
        message = Some(s"MaxCount violation. Expected $maxCount, obtained: $count"),
        sourceConstraint = None)
        
  def nodeKindError(focusNode: RDFNode, predicate: IRI, maxCount: Int, count: Int) = 
    ViolationError(id = sh + "maxCountError",
        focusNode = focusNode,
        subject = Some(focusNode),
        predicate = Some(predicate),
        obj = None,
        message = Some(s"MaxCount violation. Expected $maxCount, obtained: $count"),
        sourceConstraint = None)
        
  def inError(focusNode: RDFNode, values: Seq[Value]) = 
    ViolationError(id = sh + "inError",
        focusNode = focusNode,
        subject = Some(focusNode),
        predicate = None,
        obj = None,
        message = Some(s"In violation. Expected $focusNode to be in $values"),
        sourceConstraint = None)
}
