package es.weso.server

import io.circe.Json
import cats.syntax.either._
import es.weso.schema.{ErrorInfo, Result, Solution}
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

 def result2HTML(jsonResult: Json): String = {
  jsonResult.as[Result].fold(
   failure => s"Failure recovering result from ${jsonResult.spaces2}",
   result => {
    val sb = new StringBuilder
    if (result.isValid) {
     sb ++= s"<p class='valid'>Valid</p>"
     for {s <- result.solutions} {
      sb ++= solution2Html(s)
     }
    } else {
     sb ++= s"<p class='notValid'>Not Valid</p>"
     for (e <- result.errors) {
      sb ++= errorInfo2Html(e)
     }
    }
    sb.toString
   }
  )
 }

/*    sb ++= s"Solution as Json: <pre>${escape(solution0.focus.getOrElse(Json.Null).spaces2)}"
    val solutionNodes = solution0.fields.getOrElse(List())
    sb ++= "<table><tr><td>Node</td><td>Shapes</td><td>Evidences</td></tr>"
    for (node <- solutionNodes) {
      val nodeCursor = solution0.downField(node)
      val shapes = nodeCursor.downField("hasShapes").fields
      for (shape <- shapes) {
       sb ++= s"<tr><td>$node</td><td>$shape</td></tr>"
      }
    }
    sb ++= "</table>"
   }
   case Right(false) => {
    sb ++= s"<p class='notValid'>Not valid</p>"
    sb ++= s"<pre>${escape(result.spaces2)}</pre>"
   }
  }
  sb.toString
 } */

 def solution2Html(solution: Solution): String = {
   val sb = new StringBuilder
  sb ++= "<h3>Solution</h3>"
  sb ++= s"<pre>${escape(solution.toString)}</pre>"
  sb.toString
 }

 def errorInfo2Html(e: ErrorInfo): String = {
  val sb = new StringBuilder
  sb ++= "<h3>Error</h3>"
  sb ++= s"<pre>${escape(e.toString)}</pre>"
  sb.toString
 }
}

