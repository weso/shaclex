package es.weso.shex.report

import org.scalatest.FunSpec
import com.typesafe.config._
import java.io._
import java.nio.file.Paths

import org.scalatest.Matchers
import es.weso.shex.manifest.ManifestPrefixes._
import es.weso.rdf.RDFReader
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.parser.RDFParser
import es.weso.shapeMaps.{Start => StartMap, IRILabel => IRIMapLabel, _}
import es.weso.shex.validator.Validator
import es.weso.shex._

import scala.io.Source

class ReportGeneratorCompatTest extends FunSpec with Matchers with RDFParser {

  // If the following variable is None, it runs all tests
  // Otherwise, it runs only the test whose name is equal to the value of this variable
  val nameIfSingle: Option[String] =
  // None
  Some("0focusIRI_other")

  val conf: Config = ConfigFactory.load()
  val manifestFile = new File(conf.getString("manifestFile"))
  val outFile = conf.getString("EarlReportFile")
  val baseIRI: Option[String] = Some(Paths.get(manifestFile.getCanonicalPath()).normalize().toUri.toString)

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

  def prepareReport(rdf: RDFReader): Report = {
    val base = Paths.get(".").toUri
    val report = Report.empty


    // Validation tests
    for (triple <- rdf.triplesWithType(sht_ValidationTest)) {
      val node = triple.subj
      val nodeStr = node.getLexicalForm
      val name = rdf.triplesWithSubjectPredicate(node, mf_name).map(_.obj).head.getLexicalForm
      if (nameIfSingle == None || nameIfSingle.getOrElse("") === name) {
        it(s"validationTest: $name") {
          val tryReport = for {
            name      <- stringFromPredicate(mf_name)(node, rdf)
            action    <- objectFromPredicate(mf_action)(node, rdf)
            schemaIRI <- iriFromPredicate(sht_schema)(action, rdf)
            schemaStr = Source.fromURI(base.resolve(schemaIRI.uri))("UTF-8").mkString
            schema  <- Schema.fromString(schemaStr, "SHEXC", baseIRI, RDFAsJenaModel.empty)
            dataIRI <- iriFromPredicate(sht_data)(action, rdf)
            strData = Source.fromURI(base.resolve(dataIRI.uri))("UTF-8").mkString
            data           <- RDFAsJenaModel.fromChars(strData, "TURTLE", baseIRI)
            focus          <- objectFromPredicate(sht_focus)(action, rdf)
            maybeShape          <- iriFromPredicateOptional(sht_shape)(action, rdf)
            lbl = maybeShape.fold(StartMap: ShapeMapLabel)(IRIMapLabel(_))
            shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
            resultShapeMap <- Validator(schema).validateShapeMap(data, shapeMap)
            ok <- if (resultShapeMap.getConformantShapes(focus) contains lbl)
              Right(s"Focus $focus conforms to $lbl")
            else Left(s"Focus $focus does not conform to shape $lbl\nResultMap:\n$resultShapeMap" ++
              s"\nData: \n${strData}\nSchema: ${schemaStr}\n" ++
              s"${resultShapeMap.getInfo(focus,lbl)}\n" ++
              s"Schema: ${schema}\n" ++
              s"Data: ${data}"
            )
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
    for (triple <- rdf.triplesWithType(sht_ValidationFailure)) {
      val node    = triple.subj
      val nodeStr = node.getLexicalForm
      val name    = rdf.triplesWithSubjectPredicate(node, mf_name).map(_.obj).head.getLexicalForm
      if (nameIfSingle == None || nameIfSingle.getOrElse("") === name) {
        it(s"ValidationFailureTest: $name") {
        val tryReport = for {
          name      <- stringFromPredicate(mf_name)(node, rdf)
          action    <- objectFromPredicate(mf_action)(node, rdf)
          schemaIRI <- iriFromPredicate(sht_schema)(action, rdf)
          str = Source.fromURI(base.resolve(schemaIRI.uri))("UTF-8").mkString
          schema  <- Schema.fromString(str, "SHEXC", baseIRI, RDFAsJenaModel.empty)
          dataIRI <- iriFromPredicate(sht_data)(action, rdf)
          strData = Source.fromURI(base.resolve(dataIRI.uri))("UTF-8").mkString
          data           <- RDFAsJenaModel.fromChars(strData, "TURTLE", baseIRI)
          focus          <- objectFromPredicate(sht_focus)(action, rdf)
          maybeShape     <- iriFromPredicateOptional(sht_shape)(action, rdf)
          lbl = maybeShape.fold(StartMap: ShapeMapLabel)(IRIMapLabel(_))
          shapeMap = FixedShapeMap(Map(focus -> Map(lbl -> Info())), data.getPrefixMap, schema.prefixMap)
          resultShapeMap <- Validator(schema).validateShapeMap(data, shapeMap)
          ok <- if (resultShapeMap.getNonConformantShapes(focus) contains lbl)
            Right(s"Focus $focus does not conforms to $lbl as expected")
          else
            Left(s"Focus $focus does conform to shape $lbl and should not\nResultMap:\n$resultShapeMap")
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
    report
  }

}