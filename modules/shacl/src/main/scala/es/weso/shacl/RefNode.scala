package es.weso.shacl

import es.weso.rdf.nodes.RDFNode

case class RefNode(id: RDFNode) extends AnyVal {
  def showId = id.toString
}
