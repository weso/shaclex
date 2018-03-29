package es.weso.shacl.report

sealed abstract class Severity
case object ViolationSeverity extends Severity
case object WarningSeverity extends Severity
case object InfoSeverity extends Severity

object Severity {
  val defaultSeverity = InfoSeverity
}
