package es.weso.rdf.nodes

case class BNode(id: String) extends RDFNode {

  // @deprecated
  def newBNodeId: BNode = {
    val n = id.drop(1).toInt + 1
    BNode("b" + n)
  }

  override def toString: String = {
    "_:" + id
  }

  override def getLexicalForm = id

}


