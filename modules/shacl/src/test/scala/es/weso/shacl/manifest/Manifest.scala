package es.weso.shacl.manifest

import es.weso.rdf.nodes._
import ManifestPrefixes._

case class Manifest(
  label: Option[String],
  comment: Option[String],
  entries: List[Entry],
  includes: List[(RDFNode, Option[Manifest])])

object Manifest {
  def empty: Manifest = Manifest(None, None, List(), List())
}

case class Entry(
  node: RDFNode,
  entryType: EntryType,
  name: Option[String],
  action: ManifestAction,
  result: Result,
  status: Status,
  specRef: Option[IRI])

sealed trait EntryType {
  def iri: IRI
}
final case object Validate extends EntryType {
  override def iri = sht_Validate
}
final case object MatchNodeShape extends EntryType {
  override def iri = sht_MatchNodeShape
}
final case object ValidationFailure extends EntryType {
  override def iri = sht_ValidationFailure
}
final case object WellFormedSchema extends EntryType {
  override def iri = sht_WellFormedSchema
}
final case object NonWellFormedSchema extends EntryType {
  override def iri = sht_NonWellFormedSchema
}
final case object ConvertSchemaSyntax extends EntryType {
  override def iri = sht_ConvertSchemaSyntax
}

case class ManifestAction(
  schema: Option[IRI],
  schemaFormat: Option[String],
  data: Option[IRI],
  dataFormat: Option[String],
  schemaOutputFormat: Option[IRI],
  triggerMode: Option[IRI],
  node: Option[IRI],
  shape: Option[IRI]) {
  def setSchema(iri: IRI): ManifestAction = {
    this.copy(schema = Some(iri))
  }

  def setData(iri: IRI): ManifestAction = {
    this.copy(data = Some(iri))
  }
}

object ManifestAction {

  def apply(): ManifestAction = {
    ManifestAction(
      schema = None,
      schemaFormat = None,
      data = None,
      dataFormat = None,
      schemaOutputFormat = None,
      triggerMode = None,
      node = None,
      shape = None)
  }
}

sealed trait Result {

  def asBoolean: Option[Boolean] = {
    this match {
      case BooleanResult(b) => Some(b)
      case _ => None
    }
  }

  val isValid: Boolean
}

final case class ReportResult(report: ValidationReport) extends Result {
  override val isValid = false
}

case class ValidPair(
  node: RDFNode,
  shape: RDFNode)

final case class BooleanResult(
  value: Boolean) extends Result {
  override val isValid = value
}

final case class IRIResult(
  value: IRI) extends Result {
  override val isValid = false
}

final case object EmptyResult
  extends Result {
  override val isValid = true
}

final case class ValidationReport(violationErrors: Set[ViolationError]) {
  def failingNodes: Set[RDFNode] =
    violationErrors.map(_.focusNode).flatten

  def failingNodesShapes: List[(RDFNode,IRI)] =
    violationErrors.toList.collect {
      case v if v.focusNode.isDefined && v.sourceShape.isDefined =>
        (v.focusNode.get,v.sourceShape.get)
    }
}

final case class ViolationError(
  errorType: Option[IRI],
  focusNode: Option[RDFNode],
  path: Option[IRI],
  severity: Option[IRI],
  sourceConstraintComponent: Option[IRI],
  sourceShape: Option[IRI],
  value: Option[RDFNode])

final case class Status(value: IRI)

