package es.weso.shex.report

import org.scalatest.FunSpec
import com.typesafe.config._
import org.apache.jena.rdf.model.ModelFactory
import java.io.FileOutputStream
import java.io.FileInputStream

import scala.collection.JavaConverters._
import org.scalatest.Matchers
import es.weso.manifest._
import es.weso.manifest.ManifestPrefixes._

import scala.io.Source
import scala.util.{Failure, Success}

class ReportGenerator extends FunSpec with Matchers {

  val conf: Config = ConfigFactory.load()
  val manifestFile = conf.getString("manifestFile")
  val outFile = conf.getString("EarlReportFile")

  describe("Generate W3c EARL report") {
    RDF2Manifest.read(manifestFile,"TURTLE",None) match {
        case Failure(e) => info(s"Error reading manifest file: $e")
        case Success(manifest) => {
          val report = prepareReport(manifest)
          val earlModel = report.generateEARL
          earlModel.write(new FileOutputStream(outFile), "TURTLE")
          info(s"Report written to $outFile")
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

  def prepareReport(manifest: Manifest): Report = {
    val report = Report.empty
    for (entry <- manifest.entries) {
      val name = entry.name
        val testReport = SingleTestReport(
          passed = false,
          name = name,
          uriTest = entry.node.toIRI.str,
          testType = entry.entryType.iri.str,
          moreInfo = "Not tested yet"
        )
        report.addTestReport(testReport)
    }
    report
  }
}