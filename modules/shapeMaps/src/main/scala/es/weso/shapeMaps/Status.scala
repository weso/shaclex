package es.weso.shapeMaps

import io.circe.{ Encoder, Json }

abstract sealed class Status
case object Conformant extends Status
case object NonConformant extends Status
case object Undefined extends Status

object Status {
  implicit val encodeStatus: Encoder[Status] = new Encoder[Status] {
    final def apply(a: Status): Json = {
      a match {
        case Conformant => Json.fromString("conformant")
        case NonConformant => Json.fromString("nonconformant")
        case Undefined => Json.fromString("undefined")
      }
    }
  }

}