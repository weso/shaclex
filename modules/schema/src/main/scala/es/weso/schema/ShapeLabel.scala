package es.weso.schema
import cats._, data._
import cats.implicits

case class SchemaLabel(str: String) {
  def show: String = str
//  def toHTML(pm: PrefixMap): String = "<span class=\"shape\">" + code(str) + "</span>"
}

object SchemaLabel {
  implicit val showSchemaLabel = new Show[SchemaLabel] {
    override def show(s: SchemaLabel): String = s.show
  }
}