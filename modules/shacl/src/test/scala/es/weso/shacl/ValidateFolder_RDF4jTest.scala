package es.weso.shacl

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.nodes.IRI
import es.weso.rdf.rdf4j.RDFAsRDF4jModel
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.validator.Validator
import es.weso.utils.FileUtils._
import org.scalatest._
import scala.io.Source
import scala.util._
import cats.implicits._

class ValidateFolder_RDF4jTest extends FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclTests")

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

  def validate(name: String, str: String): Unit = {
    val attempt = for {
      rdf <- RDFAsRDF4jModel.fromChars(str, "TURTLE", Some(IRI("http://example.org/")))
      schema <- RDF2Shacl.getShacl(rdf)
      result <- Validator.validate(schema, rdf).leftMap(_.toString)
    } yield result
    attempt match {
      case Left(e) => {
        fail(s"Error validating $name: $e")
      }
      case Right(typing) => ()
    }
  }

}
