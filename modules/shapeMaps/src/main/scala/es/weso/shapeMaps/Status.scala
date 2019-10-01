package es.weso.shapeMaps

import io.circe._

abstract sealed class Status
case object Conformant extends Status
case object NonConformant extends Status
case object Undefined extends Status

object Status {
  val default = Conformant

  implicit val encodeStatus: Encoder[Status] = new Encoder[Status] {
    final def apply(a: Status): Json = {
      a match {
        case Conformant => Json.fromString("conformant")
        case NonConformant => Json.fromString("nonconformant")
        case Undefined => Json.fromString("undefined")
      }
    }
  }

  implicit val decodeStatus: Decoder[Status] = Decoder.instance { c =>
    for {
     str <- c.as[String]
     status <- str.toLowerCase match {
       case "conformant" => Right(Conformant)
       case "nonconformant" => Right(NonConformant)
       case "undefined" => Right(Undefined)
       case _ => Left(DecodingFailure(s"Unknwon value for status: $str ",Nil))
     }
    } yield status
  }

}