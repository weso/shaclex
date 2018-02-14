package es.weso.shex.compact

import java.io.File

import cats._
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.json.JsonTest
import es.weso.shex._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.eqShEx._
import es.weso.utils.FileUtils._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.{EitherValues, FunSpec, Matchers}
import es.weso.shex.implicits.encoderShEx._

import scala.io._

class CompareSchemasSingleCompatTest extends FunSpec with JsonTest with Matchers with EitherValues {

  val name = "0focusBNODE"
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
          decode[Schema](jsonStr) match {
            case Left(err) => fail(s"Error parsing $jsonFile: $err")
            case Right(expectedSchema) =>
              if (Eq[Schema].eqv(schema, expectedSchema)) {
                info("Schemas are equal")
              } else {
                fail(s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}\nParsed as Json:\n${schema.asJson.spaces2}\nExpected as Json:\n${expectedSchema.asJson.spaces2}")
              }
          }
        }
        case Left(err) => fail(s"Parsing error: $err")
      }
    }
  }
}
