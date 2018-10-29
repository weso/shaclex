package es.weso.shex.manifest

import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.parser._
import io.circe.syntax._
import org.scalatest._
import scala.io.Source

class JsonResultTest extends FunSpec with Matchers with TryValues with OptionValues{

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("validationFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri

  describe(s"ShapeResult parsing") {
    val jsonStr = "{\"shape\": \"http://schema.example/IssueShape\", \"result\": true}"
    decode[ShapeResult](jsonStr).fold(
      e => fail(s"Error parsing: $e"),
      result => info(s"Result parsed: $result")
    )
  }


  describe("ResultParsing") {
   val name = "node_kind_example_results.json"
    it(s"Should parse $name") {
    val jsonStr = Source.fromURI(shexFolderURI.resolve(name)).mkString
    decode[JsonResult](jsonStr).fold(
      e => fail(s"Error: $e"),
      result => info(s"Result parsed: ${result.asJson.spaces2}"))
   }
  }
}
