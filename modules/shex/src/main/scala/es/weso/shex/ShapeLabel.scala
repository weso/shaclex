package es.weso.shex

import es.weso.rdf.nodes.{BNode, IRI, RDFNode}

abstract sealed trait ShapeLabel {
  def toRDFNode: RDFNode = this match {
    case IRILabel(iri) => iri
    case BNodeLabel(bn) => bn
  }
}
case class IRILabel(iri: IRI) extends ShapeLabel
case class BNodeLabel(bnode: BNode) extends ShapeLabel
