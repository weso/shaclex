package es.weso.server

import io.circe.Json

case class SchemaInfoResult(
  schema: String,
  schemaFormat: String,
  schemaEngine: String,
  shapes: Json,
  prefixMap: Json)