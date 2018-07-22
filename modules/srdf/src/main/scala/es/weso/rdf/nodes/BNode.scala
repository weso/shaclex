package es.weso.rdf.nodes

import cats.Show

case class BNode(id: String) extends RDFNode {

  /* @deprecated */
  def newBNodeId: BNode = {
    val n = id.drop(1).toInt + 1
    BNode("b" + n)
  }

  override def toString: String = {
    Show[BNode].show(this)
  }

  override def getLexicalForm = id

  def isEqualTo(other: RDFNode): Boolean = other match {
    case BNode(i) => i == id
    case _ => false
  }

  def lessThan(other: RDFNode): Boolean = throw new Exception("Unimplemented lessThan")

}

object BNode {

  implicit val showBNode: Show[BNode] = new Show[BNode] {
    def show(b: BNode): String =
      "_:" + b.id
  }

}


