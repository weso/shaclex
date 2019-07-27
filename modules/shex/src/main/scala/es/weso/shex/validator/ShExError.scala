package es.weso.shex
import cats._

abstract class ShExError


case class StringError(msg: String) extends ShExError {
  override def toString = ShExError.showViolationError.show(this)
}

object ShExError {

  def msgErr(msg: String): ShExError = StringError(msg)

  implicit def showViolationError = new Show[ShExError] {
    override def show(e: ShExError): String = e match {
      case StringError(s) =>  s"Error: ${s}"
   }
  }
}

