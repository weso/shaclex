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
import es.weso.utils.MyFileUtils._
import es.weso.manifest.{Entry => ManifestEntry, _}
import java.net._

class ShaclCore extends FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclCore")
  val fileName = shaclFolder + "/manifest.ttl"

  describe(s"Validate shacl Core from manifest file located at $fileName") {
    RDF2Manifest.read(fileName, shaclFolder) match {
      case Failure(e) => println(s"Error reading manifest file:$e")
      case Success(m) => {
        processManifest(m)
      }
    }
  }

  def processManifest(m: Manifest): Unit = {
    for (e <- m.entries)
      processEntry(e)
  }

  def processEntry(e: ManifestEntry): Unit = {
    it(s"Should check entry ${e.name}") {
      val schema = getSchema(e.action)
      info("Processing entry")
    }
  }

  def getSchema(a: ManifestAction): Try[Schema] = {
    val dataFormat = a.dataFormat.getOrElse(Shacl.defaultFormat)
    a.data match {
      case None => Success(Schema.empty)
      case Some(iri) => {
        println(s"iri: ${iri.str}")
        for {
          rdf <- RDFAsJenaModel.fromURI(iri.str, dataFormat)
          schema <- RDF2Shacl.getShacl(rdf)
        } yield schema
      }
    }
  }

  def validate(str: String): Unit = {
    RDFAsJenaModel.fromChars(str, "TURTLE") match {
      case Failure(e) => fail(s"Error: $e\nCannot parse as RDF. String: \n$str")
      case Success(rdf) => RDF2Shacl.getShacl(rdf) match {
        case Failure(e) => fail(s"Error: $e\nCannot get Schema. RDF:\n${rdf.serialize("TURTLE")}")
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
