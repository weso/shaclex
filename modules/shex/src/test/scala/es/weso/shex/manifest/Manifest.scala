package es.weso.shex.manifest

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

abstract trait Entry {
  def node: RDFNode
  def entryType: IRI
  def status: Status
  def name: String
}

case class RepresentationTest(override val node: RDFNode,
                              override val status: Status,
                              override val name: String,
                              json: IRI,
                              shex: IRI,
                              ttl: IRI) extends Entry {
  override val entryType = sht_RepresentationTest
}

case class Validate(override val node: RDFNode,
                    override val status: Status,
                    override val name: String,
                    action: ManifestAction,
                    result: Result,
                    specRef: Option[IRI]
                   ) extends Entry {
  override val entryType = sht_Validate
}

abstract class ValidOrFailureTest(override val node: RDFNode,
                              override val status: Status,
                              override val name: String,
                              val traits: List[IRI],
                              val comment: String,
                              val action: Action,
                              val maybeResult: Option[IRI],
                              val entryType : IRI
                              ) extends Entry {
}

case class ValidationTest(override val node: RDFNode,
                    override val status: Status,
                    override val name: String,
                    override val traits: List[IRI],
                    override val comment: String,
                    override val action: Action,
                    override val maybeResult: Option[IRI]
                   ) extends
  ValidOrFailureTest(node,status,name,traits,comment,action,maybeResult, sht_ValidationTest) {
}

case class ValidationFailure(override val node: RDFNode,
                             override val status: Status,
                             override val name: String,
                             override val traits: List[IRI],
                             override val comment: String,
                             override val action: Action,
                             override val maybeResult: Option[IRI]
                            ) extends ValidOrFailureTest(node,status,name,traits,comment,action,maybeResult,sht_Validate) {
}

sealed trait Action
case class FocusAction(data: IRI,
                       schema: IRI,
                       focus: RDFNode,
                       shape: Option[RDFNode],
                       shapeExterns: Option[IRI]
                      ) extends Action

case class MapResultAction(data: IRI,
                           schema: IRI,
                           shapeMap: IRI
                          ) extends Action

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
  ) extends Action {
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
    case ResultShapeMapIRI(iri) => Some(iri)
    case _ => None
  }
}

final case class ResultShapeMapIRI(iri: IRI) extends Result {
  override val isValid = false
}

case class ValidPair(
  node: RDFNode,
  shape: RDFNode)

final case class BooleanResult(value: Boolean) extends Result {
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