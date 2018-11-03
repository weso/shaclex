package es.weso.shex.report

import org.scalatest.FunSpec
import com.typesafe.config._
import java.io._
import java.net.URI
import java.nio.file.Paths
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import org.scalatest.Matchers
import es.weso.shex.manifest.ManifestPrefixes._
import es.weso.rdf.PrefixMap
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI}
import es.weso.rdf.parser.RDFParser
import es.weso.shapeMaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMap, _}
import es.weso.shex.validator.{ExternalIRIResolver, Validator}
import es.weso.shex._
import es.weso.shex.compact.CompareSchemas
import es.weso.shex.manifest._
import io.circe.parser.{decode, parse}
import io.circe.syntax._
import scala.io.Source


// TODO: Remove duplication between ValidationTest and ValidationFailureTest

class ReportManifestCompatTest extends FunSpec with Matchers with RDFParser {

  val nameIfSingle: Option[String] =
  //    Some("bnode1dot_fail-missing")
    None

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")
  val validationFolder = conf.getString("validationFolder")
  val manifestFile = new File(conf.getString("manifestFile"))
  val outFile = conf.getString("EarlReportFile")
  val base = Paths.get(".").toUri
  val baseIRI: Option[IRI] = Some(IRI(Paths.get(manifestFile.getCanonicalPath()).normalize().toUri))

  describe("Generate W3c EARL report") {
    val r1 = processManifest(schemasFolder, Report.empty)
    val r2 = processManifest(validationFolder, r1)
    info(s"End of manifest processing: ${r2.items.length} items")
    val earlModel = r2.generateEARL
    earlModel.write(new FileOutputStream(outFile), "TURTLE")
    info(s"Report written in file: ${outFile}")
  }

  def processManifest(folder: String, report: Report): Report = {
    val folderURI = Paths.get(folder).normalize.toUri
    val r = RDF2Manifest.read(folder + "/" + "manifest.ttl", "Turtle", Some(folderURI.toString), false)
    r.fold(e => { println(s"Error reading schemas manifest: $e"); report },
      mf => processEntries(folderURI)(mf.entries,report)
    )
  }

  def processEntry(folderURI:URI)(report: Report, e: Entry): Report = {
    if (nameIfSingle == None || nameIfSingle.getOrElse("") === e.name) {
      val eitherValue: Either[String, String] = e match {
      case r: RepresentationTest => {
            val resolvedJsonIri = IRI(folderURI).resolve(r.json).uri
            val resolvedShExIri = IRI(folderURI).resolve(r.shex).uri
            // info(s"Entry: $r with json: ${resolvedJsonIri}")
            val jsonStr   = Source.fromURI(resolvedJsonIri)("UTF-8").mkString
            val schemaStr = Source.fromURI(resolvedShExIri)("UTF-8").mkString
            Schema.fromString(schemaStr, "SHEXC", None) match {
              case Right(schema) => {
                decode[Schema](jsonStr) match {
                  case Left(err) => fail(s"Error parsing Json ${r.json}: $err")
                  case Right(expectedSchema) =>
                    if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
                      parse(jsonStr) match {
                        case Left(err) => Left(s"Schemas are equal but error parsing Json $jsonStr")
                        case Right(json) => {
                          if (json.equals(schema.asJson)) {
                            Right("Schemas and Json representations are equal")
                          } else {
                            Left(
                              s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}")
                          }
                        }
                      }
                    } else {
                      Left(s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}")
                    }
                }
              }
              case Left(e) => Left(s"Error parsing Schema: ${r.shex}: $e")
            }
          }
          case v: ValidationTest => {
            v.action match {
              case focusAction: FocusAction => validateFocusAction(focusAction, base, v, true)
              case mr: MapResultAction      => validateMapResult(mr, base, v)
              case ma: ManifestAction       => Left(s"Not implemented validate ManifestAction yet")
            }
          }
          case v: ValidationFailure => {
            v.action match {
              case focusAction: FocusAction => validateFocusAction(focusAction, base, v, false)
              case mr: MapResultAction      => validateMapResult(mr, base, v)
              case ma: ManifestAction       => Left(s"Not implemented validate ManifestAction yet")
            }
          }
        }
        val testReport = eitherValue match {
          case Right(msg) => {
            SingleTestReport(passed = true,
                             name = e.name,
                             uriTest = e.node.getLexicalForm,
                             testType = e.entryType.str,
                             moreInfo = s"$msg")
          }
          case Left(err) => {
            SingleTestReport(passed = false,
                             name = e.name,
                             uriTest = e.node.getLexicalForm,
                             testType = e.entryType.str,
                             moreInfo = s"Error:${err}")
          }
        }
        report.addTestReport(testReport)
        // eitherValue.fold(e => fail(s"Error: $e"), msg => info(s"$msg"))
      } else report
    }

  def processEntries(folderURI: URI)(es: List[Entry], report: Report): Report =
    es.foldLeft(report)(processEntry(folderURI))

  def validateFocusAction(fa: FocusAction,
                          base: URI,
                          v: ValidOrFailureTest,
                          shouldValidate: Boolean
                         ): Either[String, String] = {
    val focus = fa.focus
    val schemaStr = Source.fromURI(base.resolve(fa.schema.uri))("UTF-8").mkString
    val dataStr   = Source.fromURI(base.resolve(fa.data.uri))("UTF-8").mkString
    for {
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
            .validateShapeMap(data, shapeMap)
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
        val schemaStr         = Source.fromURI(base.resolve(mr.schema.uri))("UTF-8").mkString
        val resolvedSmap      = base.resolve(mr.shapeMap.uri)
        val resolvedResultMap = base.resolve(resultIRI.uri)
        val resultMapStr      = Source.fromURI(resolvedResultMap).mkString
        val smapStr           = Source.fromURI(resolvedSmap).mkString
        val r: Either[String, String] = for {
          sm            <- ShapeMap.fromJson(smapStr)
          schema        <- Schema.fromString(schemaStr, "SHEXC", None)
          fixedShapeMap <- ShapeMap.fixShapeMap(sm, RDFAsJenaModel.empty, PrefixMap.empty, PrefixMap.empty)
          strData = Source.fromURI(base.resolve(mr.data.uri))("UTF-8").mkString
          data           <- RDFAsJenaModel.fromChars(strData, "TURTLE", None)
          resultShapeMap <- Validator(schema).validateShapeMap(data, fixedShapeMap)
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

