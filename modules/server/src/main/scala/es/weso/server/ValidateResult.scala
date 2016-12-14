package es.weso.server

import io.circe.Json
import org.http4s._
import org.http4s.circe._
import scala.xml.Utility.escape


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
  sb ++= "<html>"
  sb ++= "<body>"
  sb ++= "<h1>Validation result</h1>"
  sb ++=s"<p>Result: <pre class='result'>${escape(result.spaces2)}</pre></p>"

  sb ++= s"<h2>Schema details</h2>"
  // sb ++= "<details>"
  sb ++= s"<p>Schema format: $schemaFormat/$schemaEngine</p>"
  sb ++=s"<p>Trigger mode: $triggerMode</p>"
  if (node.isDefined) {
   sb ++= s"Node: <code>${node.get}</code> "
  }
  if (shape.isDefined) {
   sb ++= s"Shape: <code>${escape(shape.get)}</code>"
  }
  sb ++= s"<pre class='schema'>${escape(schema)}</pre></p>"
  //  sb ++= "</details>"
  // sb ++= s"<p>Data details: <details>"
  sb ++= "<h2>Data details</h2>"
  sb ++= s"<p>Data format: $dataFormat</p>"
  sb ++= s"<pre class='data'>${escape(data)}</pre></p>"
//  sb ++= "</details>"
  sb ++= "</body>"
  sb ++= "</html>"
  sb.toString
 }

}