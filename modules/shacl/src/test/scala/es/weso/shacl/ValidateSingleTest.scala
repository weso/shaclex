package es.weso.shacl

import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.validator.Validator
import es.weso.utils.FileUtils._
import org.scalatest._
import scala.io.Source
import scala.util._
import cats.implicits._

class ValidateSingleTest extends FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

  val name = "good7"

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclTests")

  lazy val ignoreFiles: List[String] = List()

  describe("Validate single") {
    val file = getFileFromFolderWithExt(shaclFolder, name, "ttl")
    it(s"Should validate file ${name} in folder ${shaclFolder}") {
      val str = Source.fromFile(file)("UTF-8").mkString
      validate(name, str)
    }
  }

  def validate(name: String, str: String): Unit = {
    val attempt = for {
      rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
      result <- Validator.validate(schema, rdf).leftMap(_.toString)
    } yield result
    attempt match {
      case Left(e) => fail(s"Error validating $name: $e")
      case Right(result) => {
        val (typing,ok) = result
        info(s"Typing: ${typing.show}")
        val builder = RDFAsJenaModel.empty
        info(s"Validation report: ${typing.toValidationReport.toRDF(builder).getOrElse(builder).serialize("TURTLE")}")
        ok should be(true)
      }
    }
  }

}
