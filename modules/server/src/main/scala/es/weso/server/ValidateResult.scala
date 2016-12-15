package es.weso.server

import io.circe.Json
import cats.syntax.either._

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
  sb ++= result2HTML(result)

  sb ++= s"<h2>Schema details</h2>"
  sb ++= "<details>"
  sb ++= s"<p>Schema format: $schemaFormat/$schemaEngine</p>"
  sb ++=s"<p>Trigger mode: $triggerMode</p>"
  if (node.isDefined) {
   sb ++= s"Node: <code>${node.get}</code> "
  }
  if (shape.isDefined) {
   sb ++= s"Shape: <code>${escape(shape.get)}</code>"
  }
  sb ++= s"<pre class='schema'>${escape(schema)}</pre></p>"
  sb ++= "</details>"
  sb ++= s"<p>Data details: <details>"
  sb ++= "<h2>Data details</h2>"
  sb ++= s"<p>Data format: $dataFormat</p>"
  sb ++= s"<pre class='data'>${escape(data)}</pre></p>"
  sb ++= "</details>"
  sb ++= "</body>"
  sb ++= "</html>"
  sb.toString
 }

 def result2HTML(result: Json): String = {
  val sb = new StringBuilder
  val cursor = result.hcursor
  val validMsg = cursor.get[Boolean]("valid") match {
   case Left(e) => s"<p>Error in Result: $e</p>"
   case Right(true) => {
    sb ++= s"<p class='valid'>Valid</p>"
    val solution = cursor.downField("details").downN(0).downField("solution").focus
    solution match {
      case Some(json) => sb ++= s"<pre>${escape(json.spaces2)}</pre>"
      case None => sb ++= "Not found solution..."
    }

   }
   case Right(false) => {
    sb ++= s"<p class='notValid'>Not valid</p>"
    sb ++= s"<pre>${escape(result.spaces2)}</pre>"
   }
  }
  sb.toString
 }

}