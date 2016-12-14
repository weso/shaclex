package es.weso.server

import io.circe.Json
import org.http4s._
import org.http4s.circe._

case class SchemaInfoResult
 ( schema: String,
   schemaFormat: String,
   schemaEngine: String,
   shapes: Json,
   prefixMap: Json
 )