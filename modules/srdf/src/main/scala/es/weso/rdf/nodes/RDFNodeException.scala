package es.weso.rdf.nodes

case class RDFNodeException(msg: String) extends Exception {
  override def toString(): String = {
    "RDFNodeException: \"" + msg + "\""
  }
}
