package es.weso.shex.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import es.weso.shex.compact.CompareSchemas
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import io.circe.parser._
import io.circe.syntax._

import scala.io._

class ValidationManifestTest extends ValidateManifest {

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("validationFolder")
//  val shexFolder = conf.getString("shexLocalFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri.toString

  describe("RDF2ManifestLocal") {
    val r = RDF2Manifest.read(shexFolder + "/" + "manifest.ttl", "Turtle", Some(shexFolderURI), false)
    r.fold(e => println(s"Error reading manifest: $e"),
      mf => {
        for (e <- mf.entries) {
          it(s"Should pass test ${e.name}") {
            e match {
              case r: RepresentationTest => {
                val resolvedJsonIri = IRI(shexFolderURI).resolve(r.json).uri
                val resolvedShExIri = IRI(shexFolderURI).resolve(r.shex).uri
                // info(s"Entry: $r with json: ${resolvedJsonIri}")
                val jsonStr   = Source.fromURI(resolvedJsonIri)("UTF-8").mkString
                val schemaStr = Source.fromURI(resolvedShExIri)("UTF-8").mkString
                Schema.fromString(schemaStr, "SHEXC", None, RDFAsJenaModel.empty) match {
                  case Right(schema) => {
                    decode[Schema](jsonStr) match {
                      case Left(err) => fail(s"Error parsing Json ${r.json}: $err")
                      case Right(expectedSchema) =>
                        if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
                          parse(jsonStr) match {
                            case Left(err) => fail(s"Schemas are equal but error parsing Json $jsonStr")
                            case Right(json) => {
                              if (json.equals(schema.asJson)) {
                                info("Schemas and Json representations are equal")
                              } else {
                                fail(s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}")
                              }
                            }
                         }
                        } else {
                          fail(s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}")
                        }
                    }
                  }
                  case Left(e) => fail(s"Error parsing Schema: ${r.shex}: $e")
                }
              }
              case v: ValidationTest => {
                val resolvedSchemaIRI = IRI(shexFolderURI).resolve(v.action.schema).uri
                val resolvedDataIRI = IRI(shexFolderURI).resolve(v.action.data).uri
                info(s"Error: ShExFolder: \n${shexFolderURI}\nresolved: ${resolvedSchemaIRI}\nSchema: ${v.action.schema}")
/*                val schemaStr = Source.fromURI(resolvedSchemaIRI)("UTF-8").mkString
                val dataStr = Source.fromURI(resolvedDataIRI)("UTF-8").mkString
                val r = for {
                  schema <- Schema.fromString(schemaStr,"SHEXC",None, RDFAsJenaModel.empty)
                  data <- RDFAsJenaModel.fromChars(dataStr,"TURTLE",None)
                } yield ()
                r.fold(e => fail(s"Error: ShExFolder: \n${shexFolderURI}\nresolved: ${resolvedSchemaIRI} Schema: ${v.action.schema}\nError: $e"),
                  pair => info(s"ValidationTest ${v.name} passed, shexFolder: ${shexFolderURI}, ${resolvedDataIRI}"))
*/
              }
              case v: ValidationFailure => info(s"ValidationFailure: ${v.name}")
            }
          }
        }
        info(s"Manifest read OK: ${mf.entries.length} entries")
      }
    )
   }

}
