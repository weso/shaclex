package es.weso.schema
import cats._
import data._
import cats.implicits
import es.weso.rdf.PrefixMap

case class SchemaLabel(str: String, pm: PrefixMap) {
  def show: String = pm.qualifyString(str)
//  def toHTML(pm: PrefixMap): String = "<span class=\"shape\">" + code(str) + "</span>"
}

object SchemaLabel {
  implicit val showSchemaLabel = new Show[SchemaLabel] {
    override def show(s: SchemaLabel): String = s.show
  }
}