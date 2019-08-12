package es.weso.shacl

import es.weso.rdf.nodes.{IRI, Literal, RDFNode}

/**
  * Represents IRIs or Literals (no Blank nodes)
  */
trait Value {
  /**
    * `true` if `node` matches this value
    */
  def matchNode(node: RDFNode): Boolean

  /**
    * Conversion from values to RDFNode's
    */
  def rdfNode: RDFNode
}

case class IRIValue(iri: IRI) extends Value {
  override def matchNode(node: RDFNode) = {
    node match {
      case i: IRI => iri == i
      case _ => false
    }
  }

  override def rdfNode: RDFNode = iri
}

case class LiteralValue(literal: Literal) extends Value {
  override def matchNode(node: RDFNode) = {
    node match {
      case l: Literal => l == literal
      case _ => false
    }
  }

  override def rdfNode: RDFNode = literal
}
