package es.weso.shacl.validator

import java.nio.file.{Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.manifest.{Manifest, ManifestAction, Result => ManifestResult, _}
import es.weso.shacl.{Schema, SchemaMatchers, Shacl, manifest}
import org.scalatest._

import scala.util._

class ShaclCoreTest extends FunSpec with Matchers with TryValues with OptionValues with SchemaMatchers {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclCore")
  val name = "manifest.ttl"
  val shaclFolderPath = Paths.get(shaclFolder)
  val shaclFolderURI = Paths.get(shaclFolder).normalize.toUri.toString
  val absoluteIri = IRI(shaclFolderURI)
  describeManifest(absoluteIri.resolve(IRI(name)), shaclFolderPath)

  def describeManifest(name: IRI, parentFolder: Path): Unit = {
    describe(s"Validate from manifest file $name with parent: $parentFolder (lexicalForm: ${name.getLexicalForm}") {
      val fileName = Paths.get(parentFolder.toUri.resolve(name.uri)).toString
      RDF2Manifest.read(fileName, "TURTLE", Some(fileName), true) match {
        case Left(e) => {
          it(s"Fails to read $fileName") {
            fail(s"Error reading manifestTest file:$e")
          }
        }
        case Right(pair) => {
          val (m, rdfReader) = pair
          val newParent = Paths.get(parentFolder.toUri.resolve(name.uri))
          info(s"Manifest file read ${m.entries.length} entries and ${m.includes.length} includes. New parent: $newParent")
          processManifest(m, name.getLexicalForm, newParent, rdfReader)
        }
      }
    }
  }
  def processManifest(m: Manifest, name: String, parentFolder: Path, rdfManifest: RDFBuilder): Unit = {
    println(s"processManifest with ${name} and parent folder $parentFolder")
    for ((includeNode, manifest) <- m.includes) {
      println(s"Includes: includeNode: ${includeNode} ParentFolder: $parentFolder")
      includeNode match {
        case i: IRI => describeManifest(i, parentFolder)
        case _ => fail(s"Include node is not an IRI: $includeNode")
      }

    }
    for (e <- m.entries)
      processEntry(e,name,parentFolder, rdfManifest)
  }

  def processEntry(e: manifest.Entry, name: String, parentFolder: Path, rdfManifest: RDFBuilder): Unit = {
    it(s"Should check entry ${e.node.getLexicalForm} with $parentFolder") {
      getSchemaRdf(e.action, name, parentFolder,rdfManifest) match {
        case Left(f) => {
          fail(s"Error processing Entry: $e \n $f")
        }
        case Right((schema, rdf)) => {
          validate(schema, rdf, e.result,name)
        }
      }
    }
  }

  def getSchemaRdf(a: ManifestAction, fileName: String, parentFolder: Path, manifestRdf: RDFBuilder): Either[String, (Schema,RDFReader)] = for {
    pair  <- getSchema(a,fileName,parentFolder,manifestRdf)
    (schema,schemaRdf) = pair
    dataRdf <- getData(a,fileName,parentFolder,manifestRdf,schemaRdf)
  } yield (schema, dataRdf)

  def getData(a: ManifestAction,
              fileName: String,
              parentFolder: Path,
              manifestRdf: RDFReader,
              schemaRdf: RDFReader
             ): Either[String, RDFReader] = {
    println(s"####\nGet data: ${a.data}")
     a.data match {
     case None   => Right(RDFAsJenaModel.empty)
     case Some(iri) if iri.isEmpty => Right(manifestRdf)
     case Some(iri) => {
      val dataFileName = Paths.get(parentFolder.toUri.resolve(iri.uri)).toFile
      val dataFormat  = a.dataFormat.getOrElse(Shacl.defaultFormat)
      for {
        rdf <- RDFAsJenaModel.fromFile(dataFileName, dataFormat).map(_.normalizeBNodes)
      } yield rdf
     }
    }
  }

  def getSchema(a: ManifestAction, fileName: String, parentFolder: Path, manifestRdf: RDFBuilder): Either[String, (Schema, RDFReader)] = {
    info(s"Manifest action $a, fileName $fileName, parent: $parentFolder }")
    a.schema match {
      case None => {
        info(s"No data in manifestAction $a")
        Right((Schema.empty, RDFAsJenaModel.empty))
      }
      case Some(iri) if iri.isEmpty => for {
        schema <- RDF2Shacl.getShacl(manifestRdf)
        } yield (schema, manifestRdf)
      case Some(iri) => {
        val schemaFile = Paths.get(parentFolder.toUri.resolve(iri.uri)).toFile
        val schemaFormat = a.dataFormat.getOrElse(Shacl.defaultFormat)
        for {
          schemaRdf <- RDFAsJenaModel.fromFile(schemaFile, schemaFormat).map(_.normalizeBNodes)
          schema <- {
            RDF2Shacl.getShacl(schemaRdf)
          }
        } yield (schema, schemaRdf)
      }
    }
  }

  def validate(schema: Schema, rdf: RDFReader,
               expectedResult: ManifestResult,
               name: String
              ): Unit = {
    val validator = Validator(schema)
    val result = validator.validateAll(rdf)
    expectedResult match {
      case ReportResult(report) => {
        report.failingNodesShapes.foreach { case (node,shape) =>
          result.result.fold(vr => fail(s"Validating error: ${vr}"), typing => {
            val failedNodes = typing._1.getFailedValues(node)
            info(s"Checking $node with shape $shape\nFailed nodes($node) = $failedNodes\nTyping:$typing\n") // RDF:\n${rdf.serialize("N-TRIPLES")}")
            typing._1.getFailedValues(node).map(_.id) should contain (shape)
          })
        }
      }
      case BooleanResult(b) =>
        if (result.isOK == b)
          info(s"Expected result = obtainedResult") // = $b.\nResult:\n${result}")
        else {
          fail(s"Expected result($b)!= obtained result\n$result")

        }
      case _ => fail(s"Unsupported manifestTest result $result")
    }
  }

}
