package es.weso.shex.manifest

import java.io.File

import es.weso.rdf.nodes._
import ManifestPrefixes._

case class Manifest(
  label: Option[String],
  comment: Option[String],
  entries: List[Entry],
  includes: List[(RDFNode, Manifest)])

object Manifest {
  def empty: Manifest = Manifest(None, None, List(), List())
}

case class Entry(
  node: RDFNode,
  entryType: EntryType,
  name: Option[String],
  action: Option[ManifestAction],
  result: Option[Result],
  status: Status,
  json: Option[IRI],
  shex: Option[IRI],
  ttl: Option[IRI],
  specRef: Option[IRI]
)

object Entry {
  def basic(node: RDFNode, status: Status, entryType: EntryType): Entry = Entry(node,entryType,None,None,None,status,None,None,None,None)
}


sealed trait EntryType {
  def iri: IRI
}
final case object RepresentationTest extends EntryType {
  override def iri = sht_RepresentationTest
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
  shape: Option[IRI],
  shapeMap: Option[IRI],
  resultShapeMap: Option[IRI],
  focus: Option[IRI],
  ) {
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
      shape = None,
      shapeMap = None,
      focus = None,
      resultShapeMap = None
    )
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

  def resultShapeMap: Option[IRI] = this match {
    case ResultShapeMap(iri) => Some(iri)
    case _ => None
  }
}

final case class ResultShapeMap(iri: IRI) extends Result {
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


case class Status(iri: IRI)