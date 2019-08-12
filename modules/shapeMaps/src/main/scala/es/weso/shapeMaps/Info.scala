package es.weso.shapeMaps

import io.circe._
import io.circe.syntax._

case class Info(
  status: Status = Conformant,
  reason: Option[String] = None,
  appInfo: Option[Json] = None)

object Info {
  implicit val encodeInfo: Encoder[Info] = new Encoder[Info] {
    import Status._
    final def apply(i: Info): Json = Json.obj(
      ("status", i.status.asJson),
      ("reason", i.reason.asJson),
      ("appInfo", i.appInfo.asJson))
  }

  def undefined(msg: String): Info =
    Info(Undefined, Some(msg), Some(Json.fromString(msg)))
}
