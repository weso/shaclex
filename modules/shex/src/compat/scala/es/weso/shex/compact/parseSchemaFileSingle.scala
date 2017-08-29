package es.weso.shex.compact

import java.io.File
import cats._
import cats.data._
import com.typesafe.config.{ Config, ConfigFactory }
import es.weso.json.JsonTest
import es.weso.shex._
import es.weso.shex.implicits.showShEx._
import es.weso.utils.FileUtils._
import org.scalatest.{ EitherValues, FunSpec, Matchers }
import scala.io._
import scala.util.{ Failure, Success }

class ParseSchemaFileSingle extends FunSpec with JsonTest with Matchers with EitherValues {

  val name = "1focusLength-dot"
  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  describe(s"Parsing single File $name") {
    val file: File = getFileFromFolderWithExt(schemasFolder, name, "shex")
    val fileName = schemasFolder + "/" + name + ".shex"
    it(s"Should read Schema from file ${fileName}") {
      Parser.parseSchemaFromFile(fileName) match {
        case Right(schema) => info(s"Parsed:\n$schema")
        case Left(err) => fail(s"Parsing error: $err")
      }
    }
  }
}
