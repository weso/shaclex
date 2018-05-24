package es.weso.shex.compact

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.json.JsonTest
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex._
import es.weso.utils.FileUtils._
import org.scalatest.{EitherValues, FunSpec, Matchers}

import scala.io._

class OnlySyntaxCompatTest extends FunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List("coverage")

  def getCompactFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "shex", ignoreFiles)
  }

  describe("Parsing Schemas from ShEx") {
    for (file <- getCompactFiles(schemasFolder)) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        Schema.fromString(str, "SHEXC", None, RDFAsJenaModel.empty) match {
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
