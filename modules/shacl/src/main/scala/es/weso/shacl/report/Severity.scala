package es.weso.shacl.report

import es.weso.rdf.nodes.IRI
import es.weso.shacl.SHACLPrefixes._

sealed abstract class Severity {
  def toIRI: IRI
}
case object ViolationSeverity extends Severity {
  override def toIRI: IRI = sh_Violation
}
case object WarningSeverity extends Severity {
  override def toIRI: IRI = sh_Warning
}
case object InfoSeverity extends Severity {
  override def toIRI: IRI = sh_Info
}

object Severity {
  val defaultSeverity = ViolationSeverity
}
