package es.weso.shex.compact
import org.scalatest._
import com.typesafe.config._
import java.io.File

import scala.io._
import es.weso.json._
import es.weso.utils.FileUtils._
import es.weso.shex._


class ShexCompactSingle extends FunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List("coverage")

  val files: List[String] =
    List("1val1vExprRefOR3")

  def getCompactFiles(schemasDir: String): List[File] = {
    for (name <- files)
      yield getFileFromFolderWithExt(schemasDir, name, "shex")
  }

  describe("Parsing Schemas from ShEx") {
    for (file <- getCompactFiles(schemasFolder)) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        Schema.fromString(str, "SHEXC", None) match {
          case Right(schema) => {
            val (name, ext) = splitExtension(file.getName)
            // TODO: Check that parsed file equals schema file
          }
          case Left(err) => fail(s"Parsing error: $err")
        }
      }
    }
  }
}
