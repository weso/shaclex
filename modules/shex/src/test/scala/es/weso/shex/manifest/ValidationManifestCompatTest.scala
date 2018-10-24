package es.weso.shex.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import es.weso.shex.compact.CompareSchemas
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.validator.Validator
import io.circe.parser._
import io.circe.syntax._
import es.weso.shapeMaps._
import es.weso.shapeMaps.IRILabel

import scala.io._

class ValidationManifestCompatTest extends ValidateManifest {

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
                val base = Paths.get(".").toUri
                val schemaStr = Source.fromURI(base.resolve(v.action.schema.uri))("UTF-8").mkString
                val dataStr = Source.fromURI(base.resolve(v.action.data.uri))("UTF-8").mkString
                val validate = for {
                  schema <- Schema.fromString(schemaStr,"SHEXC",None, RDFAsJenaModel.empty)
                  data <- RDFAsJenaModel.fromChars(dataStr,"TURTLE",None)
                  validation <- (v.action.focus,v.action.shape) match {
                    case (None,None) => Left(s"No focus and no shape. ${v.name}")
                    case (Some(focus),Some(shape)) => {
                      val lbl = IRILabel(shape)
                      val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
                      for {
                        r <- Validator(schema).validateShapeMap(data, shapeMap)
                      } yield if (r.getConformantShapes(focus).contains(lbl))
                        Right(s"Shape $shape is conformant for node $focus as expected")
                      else
                        Left(s"Shape $shape is not conformant for node $focus and should not\nResultShapeMap: $r")
                    }
                    case (Some(focus),None) => {
                      val shapeMap = FixedShapeMap(Map(focus -> Map(Start -> Info())), data.getPrefixMap, schema.prefixMap)
                      for {
                        r <- Validator(schema).validateShapeMap(data, shapeMap)
                      } yield if (r.getConformantShapes(focus).contains(Start))
                        Right(s"Start is conformant for node $focus as expected")
                      else
                        Left(s"Start is non-conformant for node $focus and should not\nResultShapeMap: $r")
                    }
                    case (None,Some(shape)) => Left(s"Only shape. $shape, name: ${v.name}")
                  }
                  result <- validation
                } yield result
                validate.fold(e => fail(s"Error: ${v.name}: Error: $e"),
                  resultMsg => {
                    info(s"ValidationFailure ${v.name} passed")
                  })
              }
              case v: ValidationFailure => {
                val base = Paths.get(".").toUri
                val schemaStr = Source.fromURI(base.resolve(v.action.schema.uri))("UTF-8").mkString
                val dataStr = Source.fromURI(base.resolve(v.action.data.uri))("UTF-8").mkString
                val validate: Either[String,String] = for {
                  schema <- Schema.fromString(schemaStr,"SHEXC",None, RDFAsJenaModel.empty)
                  data <- RDFAsJenaModel.fromChars(dataStr,"TURTLE",None)
                  validation <- (v.action.focus,v.action.shape) match {
                    case (None,None) => Left(s"No focus and no shape. ${v.name}")
                    case (Some(focus),Some(shape)) => {
                      val lbl = IRILabel(shape)
                      val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
                      for {
                        r <- Validator(schema).validateShapeMap(data, shapeMap)
                      } yield if (r.getNonConformantShapes(focus).contains(lbl))
                        Right(s"Shape $shape is nonConformant for node $focus")
                      else
                        Left(s"Shape $shape is conformant for node $focus and should not\nResultShapeMap: $r")
                    }
                    case (Some(focus),None) => {
                      val shapeMap = FixedShapeMap(Map(focus -> Map(Start -> Info())), data.getPrefixMap, schema.prefixMap)
                      for {
                        r <- Validator(schema).validateShapeMap(data, shapeMap)
                      } yield if (r.getNonConformantShapes(focus).contains(Start))
                        Right(s"Start is non-conformant for node $focus as expected")
                      else
                        Left(s"Start is conformant for node $focus and should not\nResultShapeMap: $r")
                    }
                    case (None,Some(shape)) => Left(s"Only shape. $shape, name: ${v.name}")
                  }
                 result <- validation
                } yield result
                validate.fold(e => fail(s"Error: ${v.name}: Error: $e"),
                  resultMsg => {
                    info(s"ValidationFailure ${v.name} passed")
                  })
              }
            }
          }
        }
        info(s"Manifest read OK: ${mf.entries.length} entries")
      }
    )
   }

}
