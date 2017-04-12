package es.weso.shacl

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.converter.RDF2Shacl
import es.weso.utils.FileUtils._
import org.scalatest._

import scala.io.Source
import scala.util._

class ValidateSingle extends
  FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

  val name="qualified"

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclFolder")


  lazy val ignoreFiles: List[String] = List()

describe("Validate single") {
  val file = getFileFromFolderWithExt(shaclFolder, name, "ttl")
    it(s"Should validate file ${name}") {
       val str = Source.fromFile(file)("UTF-8").mkString
       validate(name,str)
     }
}

def validate(name: String, str: String): Unit = {
  val attempt = for {
    rdf <- RDFAsJenaModel.parseChars(str,"TURTLE")
    schema <- RDF2Shacl.getShacl(rdf)
    result <- Validator.validate(schema,rdf)
  } yield result
  attempt match {
    case Left(e) => fail(s"Error validating $name: $e")
    case Right(typing) => info(s"Validated $name")
  }
}

}
