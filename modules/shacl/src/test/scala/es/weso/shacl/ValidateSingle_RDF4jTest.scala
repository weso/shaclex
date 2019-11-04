package es.weso.shacl

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.nodes.IRI
import es.weso.rdf.rdf4j.RDFAsRDF4jModel
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.validator.Validator
import es.weso.utils.FileUtils._
import org.scalatest._
import cats.implicits._
import scala.io.Source
import scala.util._

class ValidateSingle_RDF4jTest extends FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

  val name = "good4"

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclTests")

  lazy val ignoreFiles: List[String] = List()

  describe("Validate single") {
    val file = getFileFromFolderWithExt(shaclFolder, name, "ttl")
    it(s"Should validate file ${name}") {
      val str = Source.fromFile(file)("UTF-8").mkString
      validate(name, str)
    }
  }

  def validate(name: String, str: String): Unit = {
    val attempt = for {
      rdf <- RDFAsRDF4jModel.fromChars(str, "TURTLE", Some(IRI("http://example.org/")))
      schema <- RDF2Shacl.getShacl(rdf)
      result <- Validator.validate(schema, rdf).leftMap(_.toString)
    } yield result
    attempt match {
      case Left(e) => fail(s"Error validating $name: $e")
      case Right(typing) => info(s"Validated $name")
    }
  }

}
