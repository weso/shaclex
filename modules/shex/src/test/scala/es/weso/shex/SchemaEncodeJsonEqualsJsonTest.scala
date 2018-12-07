package es.weso.shex

import java.io.File

import cats._
import cats.implicits._
import com.typesafe.config._
import es.weso.json.JsonCompare.jsonDiff
import es.weso.json._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import es.weso.utils.FileUtils._
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import io.circe.parser.parse
import org.scalatest._
import cats.syntax.either._
import scala.io._

class SchemaEncodeJsonEqualsJsonTest extends FunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List(
    "coverage",
    "representationTests",
    "open1dotclose",
    "open1dotclosecardOpt",
    "open1dotcloseCode1",
    "openopen1dotcloseCode1closeCode3",
    "openopen1dotOr1dotclose"
  )

  def getSchemaFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "shex", ignoreFiles)
  }

  describe("Parsing Schemas from Json") {
    for (file <- getSchemaFiles(schemasFolder)) {
      it(s"Should read Schema from file ${file.getName}") {
        parseSchemaEncodeJsonEqualsJson(file)
      }
    }
  }

  def parseSchemaEncodeJsonEqualsJson(file: File): Unit = {
    for {
      strSchema <- getContents(file)
      fileJson <- getFileFromFolderWithSameExt(file,".shex",".json")
      strJson <- getContents(fileJson)
      jsonExpected <- parse(strJson.toString).leftMap(e => s"Error parsing $strJson: $e")
      schema <- Schema.fromString(strSchema).leftMap(e => s"Error obtainning Schema from string: $e\nString:\n${strSchema}")
      jsonEncoded = schema.asJson
      check <- if (jsonEncoded.equals(jsonExpected)) Right(())
      else
        Left(
          s"Jsons and different: Diff=${jsonDiff(jsonExpected, jsonEncoded)}\nJson expected:\n${jsonExpected.show}\nEncoded:\n${jsonEncoded.show}\nSchema:${schema.show}")
    } yield check
  }.fold(e => fail(e), s => {})

}
