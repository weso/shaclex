package es.weso.slang
import es.weso.rdf.nodes._
import cats.implicits._

sealed trait Label {
  def toRDFNode: RDFNode

  override def toString: String = {
    s"${toRDFNode.show}"
  }
}
case class IRILabel(iri: IRI) extends Label {
  override def toRDFNode = iri
}
case class BNodeLabel(bNode: BNode) extends Label {
  override def toRDFNode = bNode
}
