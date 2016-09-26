package es.weso.shex
import cats._, data._
import implicits._

case class ViolationError(msg: String)

object ViolationError {

  def strError(msg: String) = ViolationError(msg)

  implicit def showViolationError = new Show[ViolationError] {
    override def show(e: ViolationError): String =
      s"Error: ${e.msg}"
  }
}

