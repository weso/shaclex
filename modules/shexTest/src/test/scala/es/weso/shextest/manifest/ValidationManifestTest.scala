package es.weso.shextest.manifest

import java.net.URI
import es.weso.utils.UriUtils._
import java.nio.file.Paths
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.PrefixMap
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI}
import es.weso.shapeMaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMap, _}
import es.weso.shex._
import es.weso.shex.validator.{ExternalIRIResolver, Validator}
import es.weso.shapeMaps._
import es.weso.shex.compact.CompareSchemas
import es.weso.shextest.manifest.Utils._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import ManifestPrefixes._
import scala.io._
import io.circe.parser._
import io.circe.syntax._


class ValidationManifestTest extends ValidateManifest {

  // If the following variable is None, it runs all tests
  // Otherwise, it runs only the test whose name is equal to the value of this variable
  val nameIfSingle: Option[String] =
//     Some("1val1IRIREFClosedExtra1_fail-iri2_higher")
     None

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("validationFolder")
//  val shexFolder = conf.getString("shexLocalFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri

  //println(s"ValidationManifest")

  describe("ValidationManifest") {
    val r = RDF2Manifest.read(shexFolder + "/" + "manifest.ttl", "Turtle", Some(shexFolderURI.toString), false)
    r.fold(e => println(s"Error reading manifest: $e"),
      mf => {
        println(s"Manifest read with ${mf.entries.length} entries")
        for (e <- mf.entries) {
          if (nameIfSingle == None || nameIfSingle.getOrElse("") === e.name) {
            it(s"Should pass test ${e.name}") {
              e match {
                case r: RepresentationTest => {
                  val resolvedJson = mkLocal(r.json,schemasBase,shexFolderURI)// IRI(shexFolderURI).resolve(r.json).uri
                  val resolvedShEx = mkLocal(r.shex,schemasBase,shexFolderURI)// IRI(shexFolderURI).resolve(r.shex).uri
                  // info(s"Entry: $r with json: ${resolvedJsonIri}")
                  val jsonStr   = Source.fromURI(resolvedJson)("UTF-8").mkString
                  val schemaStr = Source.fromURI(resolvedShEx)("UTF-8").mkString
                  Schema.fromString(schemaStr, "SHEXC", None) match {
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
                                  fail(
                                    s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}")
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
                  v.action match {
                    case focusAction: FocusAction => validateFocusAction(focusAction,base,v,true)
                    case mr: MapResultAction => validateMapResult(mr,base,v)
                    case ma: ManifestAction => Left(s"Not implemented validate ManifestAction yet")
                  }
                }
                case v: ValidationFailure => {
                  val base = Paths.get(".").toUri
                  val r: Either[String,String] = v.action match {
                    case focusAction: FocusAction => validateFocusAction(focusAction,base,v,false)
                    case mr: MapResultAction => validateMapResult(mr,base,v)
                    case ma: ManifestAction => Left(s"Not implemented validate ManifestAction yet")
                  }
                  r.fold(e => fail(s"Error: ${v.name}: Error: $e"), resultMsg => {
                    info(s"ValidationFailure ${v.name} passed")
                  })
                }
              }
            }
          }
       }
     info(s"Manifest read OK: ${mf.entries.length} entries")
    }
  )
  }

  def validateFocusAction(fa: FocusAction,
                          base: URI,
                          v: ValidOrFailureTest,
                          shouldValidate: Boolean
                         ): Either[String, String] = {
    val focus = fa.focus
    val schemaUri = mkLocal(fa.schema,schemasBase,shexFolderURI)
    val dataUri = mkLocal(fa.data,schemasBase,shexFolderURI)
    for {
      schemaStr <- derefUri(schemaUri)
      dataStr <- derefUri(dataUri)
      schema <- Schema.fromString(schemaStr, "SHEXC", Some(fa.schema))
      data   <- RDFAsJenaModel.fromChars(dataStr, "TURTLE", Some(fa.data))
      lbl = fa.shape match {
        case None           => StartMap: ShapeMapLabel
        case Some(i: IRI)   => IRIMapLabel(i)
        case Some(b: BNode) => BNodeMapLabel(b)
        case Some(other) => {
          IRIMapLabel(IRI(s"UnknownLabel"))
        }
      }
      ok <- if (v.traits contains sht_Greedy) {
        Right(s"Greedy")
      } else {
        val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
        for {
          resultShapeMap <- Validator(schema, ExternalIRIResolver(fa.shapeExterns))
            .validateShapeMap(data, shapeMap).toEitherS
          ok <- if (resultShapeMap.getConformantShapes(focus) contains lbl)
            if (shouldValidate) Right(s"Focus $focus conforms to $lbl as expected")
            else Left(s"Focus $focus conforms to $lbl but should not" ++
                      s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
                      s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
                      s"Schema: ${schema}\n" ++
                      s"Data: ${data}")
          else {
            if (!shouldValidate) Right(s"Focus $focus does not conforms to $lbl as expected")
            else Left(s"Focus $focus does not conform to $lbl but should" ++
              s"\nData: \n${dataStr}\nSchema: ${schemaStr}\n" ++
              s"${resultShapeMap.getInfo(focus, lbl)}\n" ++
              s"Schema: ${schema}\n" ++
              s"Data: ${data}")
          }
        } yield ok
      }
    } yield ok
  }

  def validateMapResult(mr: MapResultAction,
                        base: URI,
                        v: ValidOrFailureTest
                       ): Either[String,String] = {
    v.maybeResult match {
      case None => Left(s"No result specified")
      case Some(resultIRI) => {
        val schemaUri         = mkLocal(mr.schema, validationBase, shexFolderURI)
        val shapeMapUri       = mkLocal(mr.shapeMap, validationBase, shexFolderURI)
        val resultMapUri      = mkLocal(resultIRI, validationBase, shexFolderURI)
        val r: Either[String, String] = for {
          schemaStr      <- derefUri(schemaUri)
          resultMapStr  <- derefUri(resultMapUri)
          smapStr       <- derefUri(shapeMapUri)
          sm            <- ShapeMap.fromJson(smapStr)
          schema        <- Schema.fromString(schemaStr, "SHEXC", None)
          fixedShapeMap <- ShapeMap.fixShapeMap(sm, RDFAsJenaModel.empty, PrefixMap.empty, PrefixMap.empty)
          dataUri = mkLocal(mr.data,schemasBase,shexFolderURI)
          strData        <- derefUri(dataUri)
          data           <- RDFAsJenaModel.fromChars(strData, "TURTLE", None)
          resultShapeMap <- Validator(schema).validateShapeMap(data, fixedShapeMap).toEitherS
          jsonResult     <- JsonResult.fromJsonString(resultMapStr)
          result <- if (jsonResult.compare(resultShapeMap)) Right(s"Json results match resultShapeMap")
          else
            Left(
              s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}")
        } yield result
        r
      }
    }
  }
}

