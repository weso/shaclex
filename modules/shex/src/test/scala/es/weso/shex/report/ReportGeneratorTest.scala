package es.weso.shex.report

import org.scalatest.FunSpec
import com.typesafe.config._
import java.io._
import java.nio.file.Paths

import org.scalatest.Matchers
import es.weso.shex.manifest.ManifestPrefixes._
import es.weso.rdf.RDFReader
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI}
import es.weso.rdf.parser.RDFParser
import es.weso.shapeMaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMap, _}
import es.weso.shex.validator.{ExternalIRIResolver, Validator}
import es.weso.shex._
import es.weso.shex.manifest.JsonResult
import io.circe.syntax._

import scala.collection.mutable
import scala.io.Source


// TODO: Remove duplication between ValidationTest and ValidationFailureTest

class ReportGeneratorCompatTest extends FunSpec with Matchers with RDFParser {

  // If the following variable is None, it runs all tests
  // Otherwise, it runs only the test whose name is equal to the value of this variable
  val nameIfSingle: Option[String] =
    None
    // Some("1literalPattern_with_REGEXP_escapes_bare_pass_escapes")

  val counter = Counter()
  val conf: Config = ConfigFactory.load()
  val manifestFile = new File(conf.getString("manifestFile"))
  val outFile = conf.getString("EarlReportFile")
  val baseIRI: Option[IRI] = Some(IRI(Paths.get(manifestFile.getCanonicalPath()).normalize().toUri))

  describe("Generate W3c EARL report") {
    RDFAsJenaModel.fromFile(manifestFile, "TURTLE", baseIRI) match {
      case Left(e) => info(s"Error reading manifestTest file: $e")
      case Right(rdf) => {
        val report = prepareReport(rdf)
        val earlModel = report.generateEARL
        earlModel.write(new FileOutputStream(outFile), "TURTLE")
        //          info(s"Report written to $outFile")
        /*for (item <- report.items) {
            it(s"Should pass ${item.name}") {
              if (!item.passed) {
                fail(s"Failed: ${item.testType}: ${item.moreInfo}")
              } else {
                info(s"OK: ${item.name}")
              }
            }
           }*/
      }
    }
  }

  def prepareReport(manifestRdf: RDFReader): Report = {
    val base = Paths.get(".").toUri
    val report = Report.empty


    // Validation tests
    for (triple <- manifestRdf.triplesWithType(sht_ValidationTest)) {
      val node = triple.subj
      val nodeStr = node.getLexicalForm
      val name = manifestRdf.triplesWithSubjectPredicate(node, mf_name).map(_.obj).head.getLexicalForm
      if (nameIfSingle == None || nameIfSingle.getOrElse("") === name) {
        it(s"validationTest: $name") {
          val tryReport = for {
            name      <- stringFromPredicate(mf_name)(node, manifestRdf)
            action    <- objectFromPredicate(mf_action)(node, manifestRdf)
            traits     <- objectsFromPredicate(sht_trait)(node, manifestRdf)
            maybeResult  <- iriFromPredicateOptional(mf_result)(node, manifestRdf)
            schemaIRI <- iriFromPredicate(sht_schema)(action, manifestRdf)
            resolvedSchema = base.resolve(schemaIRI.uri)
            schemaStr = Source.fromURI(resolvedSchema)("UTF-8").mkString
            schema  <- Schema.fromString(schemaStr, "SHEXC", Some(schemaIRI)) // Some(resolvedSchema.toString))
//            _ <- { println(s"BaseIRI: $baseIRI\n$schemaIRI"); Right(())}
            dataIRI <- iriFromPredicate(sht_data)(action, manifestRdf)
            strData = Source.fromURI(base.resolve(dataIRI.uri))("UTF-8").mkString
            data           <- RDFAsJenaModel.fromChars(strData, "TURTLE", baseIRI)
            maybeFocus <- objectFromPredicateOptional(sht_focus)(action, manifestRdf)
            maybeMap  <- iriFromPredicateOptional(sht_map)(action, manifestRdf)
            maybeShape <- objectFromPredicateOptional(sht_shape)(action, manifestRdf)
            shapeExterns <- iriFromPredicateOptional(sht_shapeExterns)(action, manifestRdf)
            shapeExternsResolved = shapeExterns.map(iri => IRI(base.resolve(iri.uri)))
            lbl = maybeShape match {
              case None           => StartMap: ShapeMapLabel
              case Some(i: IRI)   => IRIMapLabel(i)
              case Some(b: BNode) => BNodeMapLabel(b)
              case Some(other) => {
                IRIMapLabel(IRI(s"UnknownLabel"))
              }
            }
            ok <- if (traits contains sht_Greedy) {
              counter.add(s"Greedy: $name")
              Right(s"Greedy")
            }
            else maybeFocus match {
              case Some(focus) => {
                val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
                for {
                  resultShapeMap <- Validator(schema, ExternalIRIResolver(shapeExternsResolved)).validateShapeMap(data, shapeMap)
                  ok <- if (resultShapeMap.getConformantShapes(focus) contains lbl)
                        Right(s"Focus $focus conforms to $lbl")
                else Left(s"Focus $focus does not conform to shape $lbl\nResultMap:\n$resultShapeMap" ++
                  s"\nData: \n${strData}\nSchema: ${schemaStr}\n" ++
                  s"${resultShapeMap.getInfo(focus,lbl)}\n" ++
                  s"Schema: ${schema}\n" ++
                  s"Data: ${data}"
                )
              } yield ok
              }
              case None => (maybeMap,maybeResult) match {
                case (Some(smapIRI), Some(resultIRI)) => {
                  val resolvedSmap = base.resolve(smapIRI.uri)
                  val resolvedResultMap = base.resolve(resultIRI.uri)
                  val resultMapStr = Source.fromURI(resolvedResultMap).mkString
                  val smapStr = Source.fromURI(resolvedSmap).mkString
                  val r: Either[String,String] = for {
                    sm <- ShapeMap.fromJson(smapStr)
                    fixedShapeMap <- ShapeMap.fixShapeMap(sm, manifestRdf, manifestRdf.getPrefixMap(),schema.prefixMap)
                    resultShapeMap <- Validator(schema).validateShapeMap(data, fixedShapeMap)
                    jsonResult <- JsonResult.fromJsonString(resultMapStr)
                    result <- if (jsonResult.compare(resultShapeMap)) Right(s"Json results match resultShapeMap")
                    else Left(s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}")
                  } yield result
                  r
                }
                case other => {
                  Left(s"No focus and no map/result: $other")
                }
              }
            }
          } yield ok

          val testReport = tryReport match {
            case Right(msg) => {
              SingleTestReport(passed = true,
                               name = name,
                               uriTest = nodeStr,
                               testType = sht_ValidationTest.str,
                               moreInfo = s"$msg")
            }
            case Left(e) => {
              SingleTestReport(passed = false,
                               name = name,
                               uriTest = node.getLexicalForm,
                               testType = sht_ValidationTest.str,
                               moreInfo = s"Error:${e}")
            }
          }
          report.addTestReport(testReport)
          tryReport.fold(e => fail(s"Error: $e"), msg => info(s"$msg"))
        }
      }
    }

    // Validation tests
    for (triple <- manifestRdf.triplesWithType(sht_ValidationFailure)) {
      val node    = triple.subj
      val nodeStr = node.getLexicalForm
      val name = manifestRdf.triplesWithSubjectPredicate(node, mf_name).map(_.obj).head.getLexicalForm
      if (nameIfSingle == None || nameIfSingle.getOrElse("") === name) {
        it(s"ValidationFailureTest: $name") {
        val tryReport = for {
          name      <- stringFromPredicate(mf_name)(node, manifestRdf)
          traits     <- objectsFromPredicate(sht_trait)(node, manifestRdf)
          maybeResult  <- iriFromPredicateOptional(mf_result)(node, manifestRdf)
          action    <- objectFromPredicate(mf_action)(node, manifestRdf)
          schemaIRI <- iriFromPredicate(sht_schema)(action, manifestRdf)
          schemaStr = Source.fromURI(base.resolve(schemaIRI.uri))("UTF-8").mkString
          schema  <- Schema.fromString(schemaStr, "SHEXC", Some(schemaIRI))
          dataIRI <- iriFromPredicate(sht_data)(action, manifestRdf)
          strData = Source.fromURI(base.resolve(dataIRI.uri))("UTF-8").mkString
          data           <- RDFAsJenaModel.fromChars(strData, "TURTLE", baseIRI)
          maybeFocus          <- objectFromPredicateOptional(sht_focus)(action, manifestRdf)
          maybeMap  <- iriFromPredicateOptional(sht_map)(action, manifestRdf)
          maybeShape     <- objectFromPredicateOptional(sht_shape)(action, manifestRdf)
          shapeExterns <- iriFromPredicateOptional(sht_shapeExterns)(action, manifestRdf)
          shapeExternsResolved = shapeExterns.map(iri => IRI(base.resolve(iri.uri)))
          lbl = maybeShape match {
            case None           => StartMap: ShapeMapLabel
            case Some(i: IRI)   => IRIMapLabel(i)
            case Some(b: BNode) => BNodeMapLabel(b)
            case Some(other) => {
              IRIMapLabel(IRI(s"UnknownLabel"))
            }
          }
          ok <- if (traits contains sht_Greedy) {
            counter.add(s"Greedy: $name")
            Right(s"Greedy")
          } else maybeFocus match {
            case Some(focus) => {
              val shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
              for {
                resultShapeMap <- Validator(schema,ExternalIRIResolver(shapeExternsResolved)).validateShapeMap(data, shapeMap)
                ok <- if (resultShapeMap.getNonConformantShapes(focus) contains lbl)
                       Right(s"Focus $focus does not conforms to $lbl as expected")
                else
                Left(s"Focus $focus does conform to shape $lbl and should not\nResultMap:\n$resultShapeMap" ++
                  s"\nData: \n${strData}\nSchema: ${schemaStr}\n" ++
                  s"${resultShapeMap.getInfo(focus,lbl)}\n" ++
                  s"Schema: ${schema}\n" ++
                  s"Data: ${data}"
                )
              } yield ok
             }
            case None => (maybeMap,maybeResult)  match {
              case (Some(smapIRI), Some(resultIRI)) => {
                val resolvedSmap = base.resolve(smapIRI.uri)
                val smapStr = Source.fromURI(resolvedSmap).mkString
                val resolvedResultMap = base.resolve(resultIRI.uri)
                val resultMapStr = Source.fromURI(resolvedResultMap).mkString
                val r: Either[String,String] = for {
                 sm <- ShapeMap.fromJson(smapStr)
                 fixedShapeMap <- ShapeMap.fixShapeMap(sm, manifestRdf,manifestRdf.getPrefixMap(),schema.prefixMap)
                 resultShapeMap <- Validator(schema).validateShapeMap(data, fixedShapeMap)
                 jsonResult <- JsonResult.fromJsonString(resultMapStr)
                 result <- if (jsonResult.compare(resultShapeMap)) Right(s"Json results match resultShapeMap")
                 else Left(s"Json results are different. Expected: ${jsonResult.asJson.spaces2}\nObtained: ${resultShapeMap.toString}")
                } yield result
                r
              }
              case other => {
                Left(s"No focus and no map/result: $other!")
              }
            }
          }
        } yield ok

        val testReport = tryReport match {
          case Right(msg) => {
              SingleTestReport(passed = true,
                               name = name,
                               uriTest = nodeStr,
                               testType = sht_ValidationTest.str,
                               moreInfo = s"Failed as expected with error: ${msg}")
          }
          case Left(e) => {
            SingleTestReport(passed = false,
                             name = name,
                             uriTest = nodeStr,
                             testType = sht_ValidationTest.str,
                             moreInfo = s"Error ${e}")
          }
        }
        report.addTestReport(testReport)
        tryReport.fold(e => fail(s"Error: $e"), msg => info(s"$msg"))
      }
     }
    }

    it(s"Inform counter") {
      info(s"Counter:\n${counter.toString}")
    }
    report
  }


}

case class Counter() {
  private val msgs: collection.mutable.Buffer[String] = mutable.Buffer()
  def add(msg: String): Unit = {
    msgs += msg
  }

  override def toString(): String = {
    msgs.mkString("\n") ++ s"\nTotal: ${msgs.size}"
  }


}
