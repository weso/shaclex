package es.weso.shex.validator
import cats._, data._
import cats.implicits._
import es.weso.shex.{ShapeExpr, ShapeLabel}
import es.weso.shex.implicits.showShEx._

case class ShapeType(shape: ShapeExpr, label: Option[ShapeLabel]) {
  def hasLabel(expectedLabel: ShapeLabel): Boolean =
    label.fold(false)(_ == expectedLabel)
}

object ShapeType {
  def apply(shape: ShapeExpr): ShapeType = ShapeType(shape,None)

  implicit lazy val showShapeType: Show[ShapeType] = new Show[ShapeType] {
    def show(n: ShapeType) =
      if (n.label.isDefined)  s"$n.label.get"
      else Show[ShapeExpr].show(n.shape)
  }

}
