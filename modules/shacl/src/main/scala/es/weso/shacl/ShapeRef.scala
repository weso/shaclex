package es.weso.shacl

import es.weso.rdf.nodes.RDFNode

case class ShapeRef(id: RDFNode) extends AnyVal {
  def showId = id.toString
}
