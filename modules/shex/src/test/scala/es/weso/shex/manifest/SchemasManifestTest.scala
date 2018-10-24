package es.weso.shex.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex._
import es.weso.shex.compact.CompareSchemas
import io.circe.parser._
import io.circe.syntax._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._

import scala.io._

class SchemasManifestTest extends ValidateManifest {

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("schemasFolder")
//  val shexFolder = conf.getString("shexLocalFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri.toString

  describe("RDF2ManifestLocal") {
    val r = RDF2Manifest.read(shexFolder + "/" + "manifest.ttl", "Turtle", Some(shexFolderURI), false)
    r.fold(e => fail(s"Error reading manifest: $e"),
      mf => {
        for (e <- mf.entries) {
          it(s"Should pass test ${e.name}") {
            e match {
              case r: RepresentationTest => {
                val base = Paths.get(".").toUri
                val schemaStr = Source.fromURI(base.resolve(r.shex.uri))("UTF-8").mkString
                val jsonStr = Source.fromURI(base.resolve(r.json.uri))("UTF-8").mkString
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
                info(s"Contents: $jsonStr")
              }
            }
          }
        }
        info(s"Manifest read OK: ${mf.entries.length} entries")
      }
    )
   }

}
