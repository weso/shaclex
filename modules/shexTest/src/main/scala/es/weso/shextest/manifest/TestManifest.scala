package es.weso.shextest.manifest
import io.circe.parser._

object TestManifest {
  def check(): Unit = {  
    val jsonStr = "{\"shape\": \"http://schema.example/IssueShape\", \"result\": true}"
/*    decode[ShapeResult](jsonStr).fold(
      e => println(s"Error parsing: $e"),
      result => println(s"Result parsed: $result")
    ) */
  }
}