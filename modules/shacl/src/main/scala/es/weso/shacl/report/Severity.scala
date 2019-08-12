package es.weso.shacl.report

import es.weso.rdf.nodes.IRI
import es.weso.shacl.SHACLPrefixes._

sealed abstract class Severity {
  def toIRI: IRI
}
case object ViolationSeverity extends Severity {
  override def toIRI: IRI = `sh:Violation`
}
case object WarningSeverity extends Severity {
  override def toIRI: IRI = `sh:Warning`
}
case object InfoSeverity extends Severity {
  override def toIRI: IRI = `sh:Info`
}
case class GenericSeverity(iri: IRI) extends Severity {
  override def toIRI: IRI = iri
}

object Severity {
  val defaultSeverity = ViolationSeverity
}
