package es.weso.shacl.validator

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.manifest.{Manifest, ManifestAction, Result => ManifestResult, _}
import es.weso.shacl.{Schema, SchemaMatchers, Shacl, manifest}
import org.scalatest._

import scala.collection.mutable
import scala.util._

class ShaclSingleTest extends FunSpec with Matchers with TryValues with OptionValues with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclCore") + "/node"
  val name = "minInclusive-003.ttl"
  val fileName = shaclFolder + "/" + name
  val shaclFolderURI = Paths.get(fileName).normalize.toUri.toString
  val absoluteIri = IRI(shaclFolderURI)

  val failed = mutable.Stack[String]()

  describe(s"Validate from manifest file $fileName") {
    println(s"SHACLFolderURI=$shaclFolderURI")
    RDF2Manifest.read(fileName, "TURTLE", Some(shaclFolderURI), true) match {
      case Left(e) => {
        it(s"Fails to read $fileName") {
          fail(s"Error reading manifestTest file:$e")
        }
      }
      case Right(pair) => {
        val (m,_) = pair
        info(s"Manifest file read ${m.entries.length} entries and ${m.includes.length} includes")
        processManifest(m, name, fileName)
      }
        info(s"@@@ Failed: $failed")
    }
  }

  def processManifest(m: Manifest, name: String, parentFolder: String): Unit = {
    // println(s"processManifest with ${name} and parent folder $parentFolder")
    for ((includeNode, manifest) <- m.includes) {
      // println(s"Include: $includeNode")
      // TODO!!! processManifest(manifest, includeNode.getLexicalForm, name)
    }
    for (e <- m.entries)
      processEntry(e,name,parentFolder)
  }

  def processEntry(e: manifest.Entry, name: String, parentFolder: String): Unit = {
    println(s"processEntry: $name\nEntry: ${e}\n---")
    it(s"Should check entry ${e.node.getLexicalForm} with $parentFolder") {
      getSchemaRdf(e.action, name, parentFolder) match {
        case Left(f) => {
          failed.push(name)
          fail(s"Error processing Entry: $e \n $f")
        }
        case Right((schema, rdf)) => {
          println(s"Schema read:\n${schema.serialize("TURTLE",None,RDFAsJenaModel.empty).getOrElse("")}\n---")
          validate(schema, rdf, e.result,name)
      }
     }
    }
  }

  def getSchemaRdf(a: ManifestAction, fileName: String, parentFolder: String): Either[String, (Schema, RDFReader)] = {
    println(s"GetSchema RDF...fileName: $fileName\nAbsoluteIRI: $absoluteIri")
    // info(s"Manifest action $a, fileName $fileName, parent: $parentFolder")
    val parentIri = absoluteIri.resolve(IRI(fileName))
    println(s"parentIri: $parentIri\na.data=${a.data}")

    val dataFormat = a.dataFormat.getOrElse(Shacl.defaultFormat)
    (a.data,a.schema) match {
      case (None,None) => {
        info(s"No data in manifestAction $a")
        Right((Schema.empty, RDFAsJenaModel.empty))
      }
      case (Some(dataIri), Some(shapesIri)) => {
        val realDataIri = if (dataIri.isEmpty) {
          // absoluteIri + fileName
          parentIri.resolve(IRI(fileName))
        } else {
          parentIri.resolve(dataIri)
          // absoluteIri.resolve(dataIri)
        }
        //println(s"iri: $dataIri, realDataIri $realDataIri")
        val realSchemaIri = if (shapesIri.isEmpty) {
          // absoluteIri + fileName
          parentIri.resolve(IRI(fileName))
        } else {
          // absoluteIri.resolve(shapesIri)
          parentIri.resolve(shapesIri)
        }
        for {
          rdf <- RDFAsJenaModel.fromURI(realDataIri.str, dataFormat)
          schemaRdf <- RDFAsJenaModel.fromURI(realSchemaIri.str, dataFormat)
          schema <- RDF2Shacl.getShacl(schemaRdf)
          _ <- { println(s"schemaRDF: ${schemaRdf.serialize("TURTLE").getOrElse("")}\n---\nSchema:\n${schema.toString}\n---"); Right(())}
        } yield (schema, rdf)
      }
      case (None, Some(shapesIri)) => {
        val realSchemaIri = if (shapesIri.isEmpty) {
          absoluteIri + fileName
        } else {
          absoluteIri.resolve(shapesIri)
        }
        for {
          schemaRdf <- RDFAsJenaModel.fromURI(realSchemaIri.str, dataFormat)
          schema <- {
            RDF2Shacl.getShacl(schemaRdf)
          }
        } yield (schema, RDFAsJenaModel.empty)
      }
      case (Some(dataIri),None) => {
        val realDataIri = if (dataIri.isEmpty) {
          absoluteIri + fileName
        } else {
          absoluteIri.resolve(dataIri)
        }
        for {
          rdf <- RDFAsJenaModel.fromURI(realDataIri.str, dataFormat)
          schema <- {
            RDF2Shacl.getShacl(rdf)
          }
        } yield (schema, rdf)
      }
    }
  }

  def validate(schema: Schema, rdf: RDFReader, expectedResult: ManifestResult, name: String): Unit = {
    // info(s"Schema: ${schema.serialize("TURTLE", RDFAsJenaModel.empty)}")
    // info(s"RDF: ${rdf.serialize("TURTLE")}")
    val validator = Validator(schema)
    val result = validator.validateAll(rdf)
    expectedResult match {
      case ReportResult(report) => {
        report.failingNodesShapes.foreach { case (node,shape) =>
          result.result.fold(vr => fail(s"Validating error: ${vr}"), typing => {
            info(s"Checking that $node fails for shape $shape")
            info(s"Typing: ${typing}")
            typing._1.getFailedValues(node).map(_.id) should contain (shape)
          })
        }
      }
      case BooleanResult(b) =>
        if (result.isOK == b)
          info(s"Expected result = obtainedResult") // = $b.\nResult:\n${result}")
        else {
          failed.push(name)
          fail(s"Expected result($b)!= obtained result\n$result")

        }
      case _ => fail(s"Unsupported manifestTest result $result")
    }
  }

}
