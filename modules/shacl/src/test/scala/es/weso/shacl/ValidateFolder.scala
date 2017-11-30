package es.weso.shacl

import com.typesafe.config.{Config, ConfigFactory}
import java.io.File

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._

import scala.io.Source
import util._
import es.weso.shacl.validator.Validator._
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.validator.Validator
import es.weso.utils.FileUtils._

class ValidateFolder extends FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclFolder")

  lazy val ignoreFiles: List[String] = List()

  def getTtlFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "ttl", ignoreFiles)
  }

  describe("Validate folder") {
    val files = getTtlFiles(shaclFolder)
    info(s"Validating files from folder $shaclFolder: $files")
    for (file <- getTtlFiles(shaclFolder)) {
      val name = file.getName
      it(s"Should validate file $name") {
        val str = Source.fromFile(file)("UTF-8").mkString
        validate(name, str)
      }
    }
  }

  def validate(name: String, str: String): Boolean = {
    val attempt = for {
      rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
      result <- Validator.validate(schema, rdf)
    } yield result
    attempt match {
      case Left(e) => {
        fail(s"Error validating $name: $e")
        false
      }
      case Right(typing) => true
    }
  }

}
