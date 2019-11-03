package es.weso.shextest.manifest

import es.weso.rdf.nodes._

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
