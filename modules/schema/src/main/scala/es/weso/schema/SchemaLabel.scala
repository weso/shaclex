package es.weso.schema
import cats._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.RDFNode

case class SchemaLabel(node: RDFNode, pm: PrefixMap = PrefixMap.empty) {
  def show: String = {
    pm.qualify(node)
  }

  def canEqual(a: Any) = a.isInstanceOf[SchemaLabel]

  override def equals(other: Any): Boolean = {
    other match {
      case s: SchemaLabel => s.canEqual(this) && s.node == node
      case _ => false
    }
  }
  //  def toHTML(pm: PrefixMap): String = "<span class=\"shape\">" + code(str) + "</span>"
}

object SchemaLabel {
  implicit val showSchemaLabel: Show[SchemaLabel] = new Show[SchemaLabel] {
    override def show(s: SchemaLabel): String = s.show
  }
}