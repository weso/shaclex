package es.weso.schema
import es.weso.rdf.PrefixMap
import cats.Show

case class ErrorInfo(str: String) {
  def show: String = "Error: " + " " + str
}

object ErrorInfo {
  implicit val showErrorInfo = new Show[ErrorInfo] {
    override def show(e: ErrorInfo): String = e.show
  }
}
