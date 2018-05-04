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

object ShapeLabel {
  def fromRDFNode(node: RDFNode): Either[String,ShapeLabel] = node match {
    case iri: IRI => Right(IRILabel(iri))
    case bn: BNode => Right(BNodeLabel(bn))
    case _ => Left(s"Cannot convert $node to ShapeLabel")
  }
}