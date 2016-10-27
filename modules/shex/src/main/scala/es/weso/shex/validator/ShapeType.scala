package es.weso.shex.validator
import cats._
import data._
import cats.implicits._
import es.weso.shex.{Schema, ShapeExpr, ShapeLabel}
import es.weso.shex.implicits.showShEx._

case class ShapeType(shape: ShapeExpr, label: Option[ShapeLabel], schema: Schema) {
  def hasLabel(expectedLabel: ShapeLabel): Boolean =
    label.fold(false)(_ == expectedLabel)
}

object ShapeType {
  def apply(shape: ShapeExpr, schema: Schema): ShapeType = ShapeType(shape, None, schema)

  implicit lazy val showShapeType: Show[ShapeType] = new Show[ShapeType] {
    override def show(s: ShapeType) =
      s.label match {
        case None => Show[ShapeExpr].show(s.shape)
        case Some(lbl) => s"${s.schema.qualify(lbl)}"
      }
  }

}
