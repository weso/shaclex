package es.weso.server

import scala.xml.Utility._

case class DataConversionResult
 ( data: String,
   dataFormat: String,
   resultFormat: String,
   result: String
 ) {

 def toHTML: String = {
  val sb = new StringBuilder
  sb ++= "<h1>Data conversion</h1>"
  sb ++=s"<p>Source format: $dataFormat. Target format: $resultFormat"
  sb ++=s"<pre>${escape(result)}</pre>"
  sb ++=s"<p>Data: <details><pre>${escape(data)}</pre></details>"
  sb.toString
 }


}