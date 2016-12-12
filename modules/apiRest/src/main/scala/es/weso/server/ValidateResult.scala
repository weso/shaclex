package es.weso.server

import io.circe.Json
import org.http4s._
import org.http4s.circe._

import scala.xml.Utility.escape

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
 ) {

 def toHTML: String = {
  val sb = new StringBuilder
  sb ++= "<h1>Validation result</h1>"
  sb ++=s"<p>RDF Data format: $dataFormat"
  sb ++=s"<p>Schema format: $schemaFormat/$schemaEngine"
  sb ++=s"<p>Trigger mode: $triggerMode "
  if (node.isDefined) {
   sb ++= s"Node: ${node.get}"
  }
  if (shape.isDefined) {
   sb ++= s"Shape: ${shape.get}"
  }
  sb ++=s"<pre>${result}</pre>"
  sb.toString
 }

}