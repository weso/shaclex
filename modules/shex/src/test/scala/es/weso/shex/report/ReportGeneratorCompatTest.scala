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
import es.weso.shex.validator.Validator
import es.weso.shex._
import scala.io.Source

class ReportGeneratorCompatTest extends FunSpec with Matchers with RDFParser {

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
    var numTests = 0
    var numPassed = 0
    var numFailed = 0
    var numErrors = 0

    val report = Report.empty
    // Validation tests
    for (triple <- rdf.triplesWithType(sht_ValidationTest)) {
      numTests += 1
      val node = triple.subj
      val nodeStr = node.getLexicalForm
      val name = rdf.triplesWithSubjectPredicate(node, mf_name).map(_.obj).head.getLexicalForm
      val tryReport = for {
        name <- stringFromPredicate(mf_name)(node, rdf)
        action <- objectFromPredicate(mf_action)(node, rdf)
        schemaIRI <- iriFromPredicate(sht_schema)(action, rdf)
        str = Source.fromURL(schemaIRI.getLexicalForm)("UTF-8").mkString
        schema <- Schema.fromString(str, "SHEXC", baseIRI,RDFAsJenaModel.empty)
        dataIRI <- iriFromPredicate(sht_data)(action, rdf)
        strData = Source.fromURL(dataIRI.getLexicalForm)("UTF-8").mkString
        data <- RDFAsJenaModel.fromChars(strData, "TURTLE", baseIRI)
        focus <- iriFromPredicate(sht_focus)(action, rdf)
        shape <- iriFromPredicate(sht_shape)(action, rdf)
      } yield Validator(schema).validateNodeShape(data, focus, shape.getLexicalForm)
      val testReport = tryReport match {
        case Right(report) => if (report.isRight) {
          numPassed += 1
          SingleTestReport(
            passed = true,
            name = name,
            uriTest = nodeStr,
            testType = sht_ValidationTest.str,
            moreInfo = s"${report.right.get.serialize("Compact")}")
        } else {
          numFailed += 1
          SingleTestReport(
            passed = false,
            name = name,
            uriTest = nodeStr,
            testType = sht_ValidationTest.str,
            moreInfo = s"${report.left.get}")
        }
        case Left(e) => {
          numErrors += 1
          SingleTestReport(
            passed = false,
            name = name,
            uriTest = node.toIRI.str,
            testType = sht_ValidationTest.str,
            moreInfo = s"Error ${e}")
        }
      }
      report.addTestReport(testReport)
    }

    // Validation tests
    for (triple <- rdf.triplesWithType(sht_ValidationFailure)) {
      numTests += 1
      val node = triple.subj
      val nodeStr = node.getLexicalForm
      val name = rdf.triplesWithSubjectPredicate(node, mf_name).map(_.obj).head.getLexicalForm
      val tryReport = for {
        name <- stringFromPredicate(mf_name)(node, rdf)
        action <- objectFromPredicate(mf_action)(node, rdf)
        schemaIRI <- iriFromPredicate(sht_schema)(action, rdf)
        str = Source.fromURL(schemaIRI.getLexicalForm)("UTF-8").mkString
        schema <- Schema.fromString(str, "SHEXC", baseIRI,RDFAsJenaModel.empty)
        dataIRI <- iriFromPredicate(sht_data)(action, rdf)
        strData = Source.fromURL(dataIRI.getLexicalForm)("UTF-8").mkString
        data <- RDFAsJenaModel.fromChars(strData, "TURTLE", baseIRI)
        focus <- iriFromPredicate(sht_focus)(action, rdf)
        shape <- iriFromPredicate(sht_shape)(action, rdf)
      } yield Validator(schema).validateNodeShape(data, focus, shape.getLexicalForm)
      val testReport = tryReport match {
        case Right(report) => if (!report.isRight) {
          numPassed += 1
          SingleTestReport(
            passed = true,
            name = name,
            uriTest = nodeStr,
            testType = sht_ValidationTest.str,
            moreInfo = s"Failed as expected with error: ${report.left.get}")
        } else {
          numFailed += 1
          SingleTestReport(
            passed = false,
            name = name,
            uriTest = nodeStr,
            testType = sht_ValidationTest.str,
            moreInfo = s"Passed but it was expected to fail. Result: ${report.right.get.serialize("Compact")}")
        }
        case Left(e) => {
          numErrors += 1
          SingleTestReport(
            passed = false,
            name = name,
            uriTest = nodeStr,
            testType = sht_ValidationTest.str,
            moreInfo = s"Error ${e}")
        }
      }
      report.addTestReport(testReport)
    }
    println(s"# tests $numTests")
    println(s"# passed $numPassed")
    println(s"# failed $numFailed")
    println(s"# errors $numErrors")
    report
  }

}