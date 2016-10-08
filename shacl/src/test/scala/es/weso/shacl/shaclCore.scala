package es.weso.shacl

import com.typesafe.config.{Config, ConfigFactory}
import java.io.File
import java.nio.file.Paths
import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import scala.io.Source
import util._
import Validator._
import es.weso.utils.MyFileUtils._
import es.weso.manifest.{Entry => ManifestEntry, Result => ManifestResult, _}
import java.net._

class ShaclCore extends FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclCore")
  val fileName = shaclFolder + "/manifest.ttl"
  val shaclFolderURI = Paths.get(shaclFolder).normalize.toUri.toString

  describe(s"Validate shacl Core from manifest file located at $fileName") {
    RDF2Manifest.read(fileName, "TURTLE", Some(shaclFolderURI)) match {
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
      getSchemaRdf(e.action) match {
        case Failure(f) => fail(s"Error processing Entry: $e \n $f")
        case Success((schema,rdf)) => validate(schema,rdf,e.result)
      }
    }
  }

  def getSchemaRdf(a: ManifestAction): Try[(Schema,RDFReader)] = {
    val dataFormat = a.dataFormat.getOrElse(Shacl.defaultFormat)
    a.data match {
      case None => Success(Schema.empty, RDFAsJenaModel.empty)
      case Some(iri) => {
        println(s"iri: ${iri.str}")
        for {
          rdf <- RDFAsJenaModel.fromURI(iri.str, dataFormat)
          schema <- RDF2Shacl.getShacl(rdf)
        } yield (schema,rdf)
      }
    }
  }

  def validate(schema: Schema, rdf: RDFReader, expectedResult: ManifestResult): Unit = {
     val validator = Validator(schema)
     val result = validator.validateAll(rdf)
     expectedResult match {
       case NotValidResult(report,pairs) => {
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
       case _ => fail(s"Unsupported manifest result $result")
     }
  }

}
