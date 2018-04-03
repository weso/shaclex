package es.weso.server

import io.circe.Json

case class DataInfoResult(
                           data: String,
                           dataFormat: String,
                           nodes: Json,
                           prefixMap: Json
                         )