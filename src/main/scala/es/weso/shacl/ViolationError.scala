package es.weso.shacl

import es.weso.rdf.nodes._

case class ViolationError(
    id: IRI,
    focusNode: RDFNode,
    subject: Option[RDFNode],
    predicate: Option[RDFNode],
    obj: Option[RDFNode],
    message: Option[String],
    sourceConstraint: Option[RDFNode]
)

object ViolationError {
  
  def unsupportedError(focusNode: RDFNode) =  
    ViolationError(
      id = IRI("unsupported"),
      focusNode = focusNode,
      subject = None,
      predicate = None,
      obj = None,
      message = Some("Unsupported SHACL feature"),
      sourceConstraint = None)
}
