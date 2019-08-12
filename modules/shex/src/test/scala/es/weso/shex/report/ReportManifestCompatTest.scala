package es.weso.shex.report

import org.scalatest.FunSpec
import com.typesafe.config._
import java.io._
import java.net.URI
import java.nio.file.Paths
import es.weso.utils.UriUtils._
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
import es.weso.shex.manifest.Utils._
import cats.syntax.either._


// TODO: Remove duplication between ValidationTest and ValidationFailureTest

class ReportManifestCompatTest extends FunSpec with Matchers with RDFParser {

  val nameIfSingle: Option[String] =
    // Some("1iriLength2")
    None

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")
  val validationFolder = conf.getString("validationFolder")
  val negativeSyntaxFolder = conf.getString("negativeSyntaxFolder")
  val negativeStructureFolder = conf.getString("negativeStructureFolder")
//  val manifestFile = new File(conf.getString("manifestFile"))
  val outFile = conf.getString("EarlReportFile")
  val base = Paths.get(".").toUri
  val validationFolderUri = Paths.get(validationFolder).normalize.toUri

  // val baseIRI: Option[IRI] = Some(IRI(Paths.get(manifestFile.getCanonicalPath()).normalize().toUri))

  describe("Generate W3c EARL report") {
    val r = processManifest(schemasFolder)(
      ( processManifest(validationFolder)(
      ( processManifest(negativeStructureFolder)
      ( processManifest(negativeSyntaxFolder)
      ( Report.empty)
      )))
     ))
    info(s"End of manifest processing: ${r.items.length} items")
    val earlModel = r.generateEARL
    earlModel.write(new FileOutputStream(outFile), "TURTLE")
    info(s"Report written in file: ${outFile}")
  }

  def processManifest(folder: String)(report: Report): Report = {
    val folderURI = Paths.get(folder).normalize.toUri
    val r = RDF2Manifest.read(folder + "/" + "manifest.ttl", "Turtle", Some(folderURI.toString), false)
    r.fold(e => { println(s"Error reading schemas manifest: $e"); report },
      mf => processEntries(folderURI)(mf.entries,report)
    )
  }

  def processEntry(folderURI:URI)(report: Report, e: Entry): Report = {
    if (nameIfSingle == None || nameIfSingle.getOrElse("") === e.name) {
      val eitherValue: Either[String, String] = e match {
        case r: NegativeStructure => {
          // val fileName  = Paths.get(r.shex.uri.getPath).getFileName.toString
          val uri       = mkLocal(r.shex, negativeStructureBase, folderURI) // folderURI.resolve(fileName)
          for {
           str <- derefUri(uri)
           schema <- Schema.fromString(str, "SHEXC", None)
           r <- schema.wellFormed match {
             case Right(str) => Left(s"Schema is well formed, but should not\nSchema: $schema\nMsg: $str")
             case Left(str)  => Right(s"Schema is not well formed: $str\nSchema: ${schema}")
           }
          } yield r
        }
        case r: NegativeSyntax => {
          val uri       = mkLocal(r.shex,negativeSyntaxBase,folderURI)
          for {
            str <- derefUri(uri)
            check <- Schema.fromString(str, "SHEXC", None) match {
              case Right(schema) => Left(s"Schema is well formed, but should not\nSchema: $schema")
              case Left(e)       => Right(s"Faiiled to parse: $e as expected")
            }
          } yield check
        }
        case r: RepresentationTest => {
          val resolvedJsonIri = mkLocal(r.json,validationBase,folderURI) // IRI(folderURI).resolve(r.json).uri
          val resolvedShExIri = mkLocal(r.shex,validationBase,folderURI) // IRI(folderURI).resolve(r.shex).uri
          for {
            jsonStr   <- derefUri(resolvedJsonIri)
            schemaStr <- derefUri(resolvedShExIri)
            schema <- Schema.fromString(schemaStr, "SHEXC", None)
            expectedSchema <- decode[Schema](jsonStr).leftMap(e => e.getMessage)
            check <- if (CompareSchemas.compareSchemas(schema, expectedSchema)) {
              parse(jsonStr) match {
                case Left(err) => Left(s"Schemas are equal but error parsing Json $jsonStr")
                case Right(json) => if (json.equals(schema.asJson)) {
                    Right("Schemas and Json representations are equal")
                  } else
                    Left(
                      s"Json's are different\nSchema:${schema}\nJson generated: ${schema.asJson.spaces2}\nExpected: ${json.spaces2}")
                }
            } else {
              Left(s"Schemas are different. Parsed:\n${schema}\n-----Expected:\n${expectedSchema}")
            }
          } yield check
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
     }
     else report
  }

  def processEntries(folderURI: URI)(es: List[Entry], report: Report): Report =
    es.foldLeft(report)(processEntry(folderURI))

  def validateFocusAction(fa: FocusAction,
                          base: URI,
                          v: ValidOrFailureTest,
                          shouldValidate: Boolean
                         ): Either[String, String] = {
    val focus = fa.focus
    val schemaUri = mkLocal(fa.schema,validationBase,validationFolderUri)
    val dataUri   = mkLocal(fa.data, validationBase, validationFolderUri)
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
        val r: Either[String, String] = for {
          schemaStr     <- derefUri(mkLocal(mr.schema, validationBase, validationFolderUri))
          resultMapStr  <- derefUri(mkLocal(mr.shapeMap, validationBase, validationFolderUri))
          smapStr       <- derefUri(mkLocal(resultIRI, validationBase, validationFolderUri))
          sm            <- ShapeMap.fromJson(smapStr)
          schema        <- Schema.fromString(schemaStr, "SHEXC", None)
          fixedShapeMap <- ShapeMap.fixShapeMap(sm, RDFAsJenaModel.empty, PrefixMap.empty, PrefixMap.empty)
          strData       <- derefUri(mkLocal(mr.data,schemasBase,validationFolderUri))
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

