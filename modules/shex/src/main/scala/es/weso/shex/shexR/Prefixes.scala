package es.weso.shex.shexR

import es.weso.rdf.nodes.IRI

/**
 * Common Prefixes for RDF
 */
object PREFIXES {

  lazy val sx = IRI("http://shex.io/ns/shex#")
  lazy val sx_Schema = sx + "Schema"

  lazy val sx_startActs = sx + "startActs"
  lazy val sx_start = sx + "start"
  lazy val sx_shapes = sx + "shapes"


  private val shexMap: Map[String, IRI] =
    Map("sx" -> sx)

}
