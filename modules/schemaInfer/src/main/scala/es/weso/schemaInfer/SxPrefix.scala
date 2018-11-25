package es.weso.schemaInfer
import es.weso.rdf.nodes.IRI

// Namespace for annotations
object SxNamespace {

  val sx = IRI("http://weso.es/ns/shex/")
  val `sx:maxNumber` = sx + "maxNumber"

}