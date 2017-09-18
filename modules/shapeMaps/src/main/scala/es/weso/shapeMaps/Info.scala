package es.weso.shapeMaps

import io.circe.Json

case class Info(
  status: Status = Conformant,
  reason: Option[String] = None,
  appInfo: Json = Json.Null)
