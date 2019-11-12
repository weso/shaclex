package es.weso.shex.compact
import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.shex._
import es.weso.shex.implicits.encoderShEx._
import es.weso.utils.FileUtils._
import es.weso.utils.json.JsonTest
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.{EitherValues, FunSpec, Matchers}

import scala.io._

class CompareJsonTest extends FunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List(
    "coverage",
    "1refbnode_with_spanning_PN_CHARS_BASE1",  // Problem with Unicode characters...
    "_all"
  )

  def getCompactFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "shex", ignoreFiles)
  }

  // We ignore this test because it is already executed in SchemasManifestTest which runs all the tests from the manifest file
  ignore("Parsing Schemas from ShEx") {
    var failedNames = List[String]()
    for (file <- getCompactFiles(schemasFolder)) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        Schema.fromString(str) match {
          case Right(schema) => {
            val (name, ext) = splitExtension(file.getName)
            val jsonFile = schemasFolder + "/" + name + ".json"
            val jsonStr = Source.fromFile(jsonFile)("UTF-8").mkString
            parse(jsonStr) match {
              case Left(err) => fail(s"Error parsing $jsonFile: $err")
              case Right(json) =>
                if (json.equals(schema.asJson)) {
                } else {
                  failedNames = failedNames ++ List(name)
                  fail(s"Json's are different") // Parsed:\n${schema.asJson.spaces2}\n-----Expected:\n${json.spaces2}")
                }
            }
          }
          case Left(err) => fail(s"Parsing error: $err")
        }
      }
    }
    info(s"Failed names:\n${failedNames.mkString("\n")}")
  }
}
