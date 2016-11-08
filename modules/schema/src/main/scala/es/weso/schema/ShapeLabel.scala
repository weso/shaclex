package es.weso.schema
import cats._, data._
import cats.implicits

case class ShapeLabel(str: String) {
  def show: String = str
//  def toHTML(pm: PrefixMap): String = "<span class=\"shape\">" + code(str) + "</span>"
}

object ShapeLabel {
  implicit val showShapeLabel = new Show[ShapeLabel] {
    override def show(s: ShapeLabel): String = s.show
  }
}