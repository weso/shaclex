package es.weso.shex.compact
import org.scalatest.{EitherValues, FunSpec, Matchers, _}
import com.typesafe.config.{Config, ConfigFactory, _}
import java.io.File

import scala.io._
import es.weso.shex.implicits.showShEx._
import es.weso.shex.compact.Parser._
import cats.implicits._
import es.weso.json.{JsonTest, _}
import es.weso.utils.FileUtils._
import es.weso.shex._
import io.circe._
import cats._, data._
import cats.implicits._
import io.circe.syntax._
import io.circe.parser._
import JsonDiff._
import scala.util.{Failure, Success}
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._

class CompareJson extends FunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List("coverage")

  def getCompactFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "shex", ignoreFiles)
  }

  describe("Parsing Schemas from ShEx") {
    for(file <- getCompactFiles(schemasFolder)) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        Schema.fromString(str,"SHEXC",None) match {
          case Success(schema) => {
            val (name,ext) = splitExtension(file.getName)
            val jsonFile = schemasFolder + "/"+ name + ".json"
            val jsonStr = Source.fromFile(jsonFile)("UTF-8").mkString
            parse(jsonStr) match {
              case Xor.Left(err) => fail(s"Error parsing $jsonFile: $err")
              case Xor.Right(json) =>
                if (json.equals(schema.asJson)) {
                 info("Jsons are equal")
                } else {
               fail(s"Json's are different. Parsed:\n${schema.asJson.spaces4}\n-----Expected:\n${json.spaces4}")
            }
          }}
          case Failure(err) => fail(s"Parsing error: $err")
        }
      }
    }
  }
}
