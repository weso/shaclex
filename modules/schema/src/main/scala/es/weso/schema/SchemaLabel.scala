package es.weso.schema
import cats._
import data._
import cats.implicits
import es.weso.rdf.PrefixMap

case class SchemaLabel(str: String, pm: PrefixMap = PrefixMap.empty) {
  def show: String = pm.qualifyString(str)

  def canEqual(a: Any) = a.isInstanceOf[SchemaLabel]

  override def equals(other: Any): Boolean = {
    other match {
      case s: SchemaLabel => s.canEqual(this) && s.str == str
      case _ => false
    }
  }
  //  def toHTML(pm: PrefixMap): String = "<span class=\"shape\">" + code(str) + "</span>"
}

object SchemaLabel {
  implicit val showSchemaLabel = new Show[SchemaLabel] {
    override def show(s: SchemaLabel): String = s.show
  }
}