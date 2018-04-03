package es.weso.server

import scala.xml.Utility.escape

case class SchemaConversionResult(
  schema: String,
  schemaFormat: String,
  schemaEngine: String,
  resultSchemaFormat: String,
  resultSchemaEngine: String,
  result: String) {

  def toHTML: String = {
    val sb = new StringBuilder
    sb ++= "<h1>Schema conversion result</h1>"
    sb ++= s"<pre>${escape(result)}</pre>"
    sb ++= s"<p>Source schema: <details>"
    sb ++= s"<p>Source format: $schemaFormat/$schemaEngine</p>"
    sb ++= s"<p>Target format: $resultSchemaFormat/$resultSchemaEngine</p>"
    sb ++= s"<pre>${escape(schema)}</pre>"
    sb ++= s"</details>"
    sb.toString
  }

}