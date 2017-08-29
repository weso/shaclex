package es.weso.server

import io.circe.Json
import org.http4s._
import org.http4s.circe._

case class DataInfoResult(
  data: String,
  dataFormat: String,
  nodes: Json,
  prefixMap: Json)