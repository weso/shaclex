package es.weso.shex.compact

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.json.{JsonCompare, JsonTest}
import es.weso.shex._
import es.weso.utils.FileUtils._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.{EitherValues, FunSpec, Matchers}
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import cats.implicits._

import scala.io._

class CompareJsonSingleCompatTest extends FunSpec with JsonTest with Matchers with EitherValues {

  val name = "1dotShapeAnnotAIRIREF"
  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  describe(s"Parsing single File $name") {
    val file: File = getFileFromFolderWithExt(schemasFolder, name, "shex")
    it(s"Should read Schema from file ${file.getName}") {
      val str = Source.fromFile(file)("UTF-8").mkString
      Schema.fromString(str, "SHEXC", None) match {
        case Right(schema) => {
          val (name, ext) = splitExtension(file.getName)
          val jsonFile = schemasFolder + "/" + name + ".json"
          val jsonStr = Source.fromFile(jsonFile)("UTF-8").mkString
          parse(jsonStr) match {
            case Left(err) => fail(s"Error parsing $jsonFile: $err")
            case Right(json) => {
              val json2 = schema.asJson
              if (json.equals(json2)) {
                info("Jsons are equal")
              } else {
                fail(s"Json's are different. Parsed:\n${schema.asJson.spaces4}\n-----Expected:\n${json.spaces4}. Diff: ${JsonCompare.diff(json,json2)}\nSchema\n${schema.show}\nPlain schema\n${schema}")
              }
            }
          }
        }
        case Left(err) => fail(s"Parsing error: $err")
      }
    }
  }
}
