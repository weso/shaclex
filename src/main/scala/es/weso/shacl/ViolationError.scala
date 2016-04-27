package es.weso.shacl

import es.weso.rdf.nodes._
import SHACLPrefixes._

case class ViolationError(
    id: IRI,
    focusNode: RDFNode,
    subject: Option[RDFNode],
    predicate: Option[RDFNode],
    obj: Option[RDFNode],
    message: Option[String],
    sourceConstraint: Option[RDFNode]
) extends Exception("Violation error")

object ViolationError {
  
  def unsupportedError(focusNode: RDFNode) =  
    ViolationError(
      id = sh + "unsupported",
      focusNode = focusNode,
      subject = None,
      predicate = None,
      obj = None,
      message = Some("Unsupported SHACL feature"),
      sourceConstraint = None)
      
  def minCountError(focusNode: RDFNode, predicate: IRI, minCount: Int, count: Int) = 
    ViolationError(id = sh + "minCountError",
        focusNode = focusNode,
        subject = Some(focusNode),
        predicate = Some(predicate),
        obj = None,
        message = Some(s"MinCount violation. Expected $minCount, obtained: $count"),
        sourceConstraint = None)
}
