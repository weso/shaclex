package es.weso.server

import io.circe.Json

import scala.xml.Utility.escape

case class SchemaConversionResult
 ( schema: String,
   schemaFormat: String,
   schemaEngine: String,
   resultSchemaFormat: String,
   resultSchemaEngine: String,
   result: String
 ) {

 def toHTML: String = {
  val sb = new StringBuilder
  sb ++= "<h1>Schema conversion result</h1>"
  sb ++=s"<p>Source format: $schemaFormat/$schemaEngine. Target format: $resultSchemaFormat/$resultSchemaEngine"
  sb ++=s"<pre>${escape(result)}</pre>"
  sb ++=s"<p>Source schema: <details><pre>${escape(schema)}</pre></details>"
  sb.toString
 }

}