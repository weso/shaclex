package es.weso.shex.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.shex._
import es.weso.shex.compact.CompareSchemas
import io.circe.parser._
import io.circe.syntax._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import Utils._
import es.weso.utils.UriUtils._
import cats.syntax.either._

class SchemasManifestTest extends ValidateManifest {

  val nameIfSingle: Option[String] =
     None
     // Some("TwoNegation_pass")

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("schemasFolder")
//  val shexFolder = conf.getString("shexLocalFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri

  describe("RDF2ManifestLocal") {
    val r = RDF2Manifest.read(shexFolder + "/" + "manifest.ttl", "Turtle", Some(shexFolderURI.toString), false)
    r.fold(e => fail(s"Error reading manifest: $e"),
      mf => {
        for (e <- mf.entries) {
          if (nameIfSingle == None || nameIfSingle.getOrElse("") === e.name) {
            it(s"Should pass test ${e.name}") {
              e match {
                case r: RepresentationTest => {
                  val base      = Paths.get(".").toUri
                  val schemaUri = mkLocal(r.shex, schemasBase, shexFolderURI)
                  val jsonUri   = mkLocal(r.json, schemasBase, shexFolderURI)
                  val either: Either[String, String] = for {
                    schemaStr      <- derefUri(schemaUri)
                    jsonStr        <- derefUri(jsonUri)
                    schema         <- Schema.fromString(schemaStr, "SHEXC", None)
                    _              <- schema.wellFormed
                    expectedSchema <- decode[Schema](jsonStr).leftMap(_.getMessage)
                    _ <- if (CompareSchemas.compareSchemas(schema, expectedSchema)) Right(())
                    else Left(s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}")
                    json <- parse(jsonStr).leftMap(e => s"Error parsing Expected JSON schema: ${e.getMessage}")
                    check <- if (json.equals(schema.asJson)) Right(s"Schemas are equal")
                    else
                      Left(
                        s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}")
                  } yield check
                  either.fold(e => fail(s"Error: $e"), msg => info(msg))
                }
              }
            }
          }
        }
        info(s"Manifest read OK: ${mf.entries.length} entries")
      }
    )
   }


}
