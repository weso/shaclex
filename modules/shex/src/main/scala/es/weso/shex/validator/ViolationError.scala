package es.weso.shex
import cats._, data._
import implicits._

case class ViolationError(msg: String) {
  override def toString = ViolationError.showViolationError.show(this)
}

object ViolationError {

  def msgErr(msg: String) = ViolationError(msg)

  implicit def showViolationError = new Show[ViolationError] {
    override def show(e: ViolationError): String =
      s"Error: ${e.msg}"
  }
}

