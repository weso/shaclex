package es.weso.server

import io.circe.Json
import org.http4s._
import org.http4s.circe._

case class ValidateResult
 ( data: String,
   dataFormat: String,
   schema: String,
   schemaFormat: String,
   schemaEngine: String,
   triggerMode: String,
   node: Option[String],
   shape: Option[String],
   result: Json
 )