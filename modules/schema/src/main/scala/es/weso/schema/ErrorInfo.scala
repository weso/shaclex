package es.weso.schema
import es.weso.rdf.PrefixMap
import cats.Show
import io.circe.JsonObject._
import io.circe.{Encoder, Json}

case class ErrorInfo(str: String) {
  def show: String = "Error: " + " " + str
}

object ErrorInfo {
  implicit val showErrorInfo = new Show[ErrorInfo] {
    override def show(e: ErrorInfo): String = e.show
  }

  implicit val encodeErrorInfo: Encoder[ErrorInfo] = new Encoder[ErrorInfo] {
    final def apply(e: ErrorInfo): Json = Json.fromJsonObject(
      singleton("type",Json.fromString("ErrorInfo")).
        add("error", Json.fromString(e.str))
    )
  }

}
