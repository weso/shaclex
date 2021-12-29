package es.weso.schema
import cats.Show
import com.typesafe.scalalogging.LazyLogging
import io.circe.JsonObject._
import io.circe.{ Decoder, Encoder, Json }

case class ErrorInfo(msg: String) {
  def show: String = msg
}

object ErrorInfo extends LazyLogging {
  implicit val showErrorInfo: Show[ErrorInfo] = new Show[ErrorInfo] {
    override def show(e: ErrorInfo): String = e.show
  }

  implicit val encodeErrorInfo: Encoder[ErrorInfo] = new Encoder[ErrorInfo] {
    final def apply(e: ErrorInfo): Json = Json.fromJsonObject(
      singleton("type", Json.fromString("ErrorInfo")).
        add("error", Json.fromString(e.msg)))
  }

  implicit val decodeErrorInfo: Decoder[ErrorInfo] = Decoder.instance { c =>
    logger.debug(s"Decoding error info: $c")
    for {
      msg <- c.get[String]("error")
    } yield ErrorInfo(msg)
  }

}
