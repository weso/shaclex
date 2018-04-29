package es.weso.shex
import cats._

case class ShExError(msg: String) {
  override def toString = ShExError.showViolationError.show(this)
}

object ShExError {

  def msgErr(msg: String) = ShExError(msg)

  implicit def showViolationError = new Show[ShExError] {
    override def show(e: ShExError): String =
      s"Error: ${e.msg}"
  }
}

