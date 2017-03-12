package es.weso.shacl

import com.typesafe.config.{Config, ConfigFactory}
import java.io.File

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._

import scala.io.Source
import util._
import Validator._
import es.weso.shacl.converter.RDF2Shacl
import es.weso.utils.FileUtils._

class ValidateFolder extends
  FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclFolder")

  lazy val ignoreFiles: List[String] = List()

  def getTtlFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "ttl", ignoreFiles)
  }

describe("Validate folder") {
    for (file <- getTtlFiles(shaclFolder)) {
      it(s"Should validate file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        validate(str)
      }
    }
}

def validate(str: String): Unit = {
  RDFAsJenaModel.fromChars(str,"TURTLE") match {
    case Failure(e) => fail(s"Error: $e\nCannot parse as RDF. String: \n$str")
    case Success(rdf) => RDF2Shacl.getShacl(rdf) match {
      case Failure(e) => fail(s"Error: $e\nCannot get Schema from RDF. String: \n${rdf.serialize("TURTLE")}")
      case Success(schema) => {
        val validator = Validator(schema)
        val result = validator.validateAll(rdf)
        if (result.isOK) info("Valid")
        else fail(s"Not valid\n${result.show}")
      }

    }
  }
}

}
