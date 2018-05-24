package es.weso.shacl

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Paths

import org.scalatest._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._

import util._
import es.weso.shacl.manifest.{Entry => ManifestEntry, Result => ManifestResult, _}
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.manifest.{Manifest, ManifestAction, Result}
import es.weso.shacl.validator.Validator

class ShaclCoreTest extends FunSpec with Matchers with TryValues with OptionValues

  with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclTests")
  val fileName = shaclFolder + "/manifestTest.ttl"
  val shaclFolderURI = Paths.get(shaclFolder).normalize.toUri.toString

  describe(s"Validate shacl Core from manifestTest file located at $fileName") {
    RDF2Manifest.read(fileName, "TURTLE", Some(shaclFolderURI), true) match {
      case Left(e) => {
        println(s"Error reading manifestTest file:$e")
      }
      case Right(m) => processManifest(m)
    }
  }

  def processManifest(m: Manifest): Unit = {
    for (e <- m.entries)
      processEntry(e)
  }

  def processEntry(e: manifest.Entry): Unit = {
    it(s"Should check entry ${e.name}") {
      getSchemaRdf(e.action) match {
        case Left(f) => fail(s"Error processing Entry: $e \n $f")
        case Right((schema, rdf)) => validate(schema, rdf, e.result)
      }
    }
  }

  def getSchemaRdf(a: ManifestAction): Either[String, (Schema, RDFReader)] = {
    val dataFormat = a.dataFormat.getOrElse(Shacl.defaultFormat)
    a.data match {
      case None => Right((Schema.empty, RDFAsJenaModel.empty))
      case Some(iri) => {
        println(s"iri: ${iri.str}")
        for {
          rdf <- RDFAsJenaModel.fromURI(iri.str, dataFormat)
          schema <- RDF2Shacl.getShacl(rdf)
        } yield (schema, rdf)
      }
    }
  }

  def validate(schema: Schema, rdf: RDFReader, expectedResult: Result): Unit = {
    val validator = Validator(schema)
    val result = validator.validateAll(rdf)
    expectedResult match {
      case ReportResult(report) => {
        if (result.isOK)
          fail(s"Valid when expected to be not valid\n${result.show}\nExpected result: $report")
        else {
          info(s"Not valid as expected: $result. Failing nodes = ${report.failingNodes}\n Errors: ${result.errors}")
        }
      }
      case BooleanResult(b) =>
        if (result.isOK == b)
          info(s"Expected result = obtainedResult = $b.\nResult:\n${result}")
        else {
          fail(s"Expected result($b)!= obtained result\n$result")
        }
      case _ => fail(s"Unsupported manifestTest result $result")
    }
  }

}
