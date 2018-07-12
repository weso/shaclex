package es.weso.shacl

import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Paths

import org.scalatest._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._

import util._
import es.weso.shacl.manifest._
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.manifest.{Manifest, ManifestAction, Result}
import es.weso.shacl.validator.Validator
import cats.implicits._
import es.weso.rdf.nodes.IRI

class ShaclCoreTest extends FunSpec with Matchers with TryValues with OptionValues

  with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclCore")
  val name = "node-001.ttl"
  val fileName = shaclFolder + "/" + name
  val shaclFolderURI = Paths.get(shaclFolder).normalize.toUri.toString
  val absoluteIri = IRI(shaclFolderURI)

  describe(s"Validate from manifest file $fileName") {
    RDF2Manifest.read(fileName, "TURTLE", Some(shaclFolderURI), true) match {
      case Left(e) => {
        fail(s"Error reading manifestTest file:$e")
      }
      case Right(m) => {
        info(s"Manifest file read ${m.entries.length} entries and ${m.includes.length} includes")
        processManifest(m, name)
      }
    }
  }

  def processManifest(m: Manifest, name: String): Unit = {
    for ((includeNode,manifest) <- m.includes) {
      println(s"Include: $includeNode")
      processManifest(manifest, includeNode.getLexicalForm)
    }
    for (e <- m.entries)
      processEntry(e,name)
  }

  def processEntry(e: manifest.Entry, name: String): Unit = {
    it(s"Should check entry ${e.node.getLexicalForm}") {
          getSchemaRdf(e.action, name) match {
            case Left(f) => fail(s"Error processing Entry: $e \n $f")
            case Right((schema, rdf)) => validate(schema, rdf, e.result)
          }
        }
  }

  def getSchemaRdf(a: ManifestAction, fileName: String): Either[String, (Schema, RDFReader)] = {
    val dataFormat = a.dataFormat.getOrElse(Shacl.defaultFormat)
    a.data match {
      case None => {
        info(s"No data in manifestAction $a")
        Right((Schema.empty, RDFAsJenaModel.empty))
      }
      case Some(iri) => {
        val realIri = if (iri.isEmpty) {
          absoluteIri + fileName
        } else {
          absoluteIri.resolve(iri)
        }
        for {
          rdf <- RDFAsJenaModel.fromURI(realIri.str, dataFormat)
          schema <- {
            RDF2Shacl.getShacl(rdf)
          }
        } yield (schema, rdf)
      }
    }
  }

  def validate(schema: Schema, rdf: RDFReader, expectedResult: Result): Unit = {
    // info(s"Schema: ${schema.serialize("TURTLE", RDFAsJenaModel.empty)}")
    // info(s"RDF: ${rdf.serialize("TURTLE")}")
    val validator = Validator(schema)
    val result = validator.validateAll(rdf)
    expectedResult match {
      case ReportResult(report) => {
        report.failingNodesShapes.foreach { case (node,shape) =>
          result.result.fold(vr => fail(s"Validating error: ${vr}"), typing => {
            info(s"Checking that $node fails for shape $shape")
            info(s"Typing: ${typing.show}")
            typing.getFailedValues(node).map(_.id) should contain (shape)
          })
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
