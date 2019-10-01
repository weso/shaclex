package es.weso.shex.converter
import java.net.URI

import es.weso.shex.Schema

case class JsonSchema(
  prefixes: List[(String,URI)]
)
object Shex2JsonSchema {

  def shex2JsonSchema(schema: Schema): JsonSchema = {
//    schema.prefixMap.
//    JsonSchema()
    ???
  }
}