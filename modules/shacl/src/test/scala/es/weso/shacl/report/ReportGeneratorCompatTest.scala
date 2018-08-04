package es.weso.shacl.report

import java.io._
import java.nio.file.Paths

import com.typesafe.config._
import es.weso.rdf.RDFReader
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.rdf.parser.RDFParser
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.{Schema, Shacl, manifest}
import es.weso.shacl.manifest._
import es.weso.shacl.validator.Validator
import org.scalatest.{FunSpec, Matchers}
import es.weso.shacl.manifest.{Manifest, ManifestAction, Result => ManifestResult, _}

import scala.util.{Either, Left, Right}

class ReportGeneratorCompatTest extends FunSpec with Matchers with RDFParser {

  val conf: Config = ConfigFactory.load()
//  val manifestFile = new File(conf.getString("manifestFile"))
  val outFile = conf.getString("EarlReportFile")
//  val baseIRI: Option[String] = Some(Paths.get(manifestFile.getCanonicalPath()).normalize().toUri.toString)

  val shaclFolder    = conf.getString("shaclCore")
  val name           = "manifest.ttl"
  val fileName       = shaclFolder + "/" + name
  val shaclFolderURI = Paths.get(shaclFolder).normalize.toUri.toString
  val absoluteIri    = IRI(shaclFolderURI)

  var numTests  = 0
  var numPassed = 0
  var numFailed = 0
  var numErrors = 0
  val report = Report.empty

    // Validation tests
    /*    for (triple <- rdf.triplesWithType(sht_ValidationTest)) {
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
    } */


    describe(s"Generate W3c EARL report from $fileName") {
      RDF2Manifest.read(fileName, "TURTLE", Some(shaclFolderURI), true) match {
        case Left(e) => {
          it(s"Fails to read $fileName") {
            fail(s"Error reading manifestTest file:$e")
          }
        }
        case Right(m) => {
          info(s"Manifest file read ${m.entries.length} entries and ${m.includes.length} includes")
          processManifest(m, name, fileName)

          it(s"Saves info in EARL file: ${outFile}") {
            val earlModel = report.generateEARL
            earlModel.write(new FileOutputStream(outFile), "TURTLE")
          }
        }
      }
    it(s"Shows counters") {
      info(s"#tests=$numTests, #passed=$numPassed, #failed=$numFailed, #errors=$numErrors")
    }
  }

  def processManifest(m: Manifest, name: String, parentFolder: String): Unit = {
      // println(s"processManifest with ${name} and parent folder $parentFolder")
      for ((includeNode, manifest) <- m.includes) {
        // println(s"Include: $includeNode")
        processManifest(manifest, includeNode.getLexicalForm, name)
      }
      for (e <- m.entries)
        processEntry(e,name,parentFolder)
    }

    def processEntry(e: manifest.Entry, name: String, parentFolder: String): Unit = {
      it(s"Should check entry ${e.node} with $parentFolder") {
        getSchemaRdf(e.action, name, parentFolder) match {
          case Left(f) => {
            numErrors += 1
            fail(s"Error processing Entry: $e \n $f")
          }
          case Right((schema, rdf)) =>
            validate(schema, rdf, e.result, name, parentFolder, e.node)
        }
      }
  }

    def getSchemaRdf(a: ManifestAction, fileName: String, parentFolder: String): Either[String, (Schema, RDFReader)] = {
      info(s"Manifest action $a, fileName $fileName, parent: $parentFolder")
      val parentIri = absoluteIri.resolve(IRI(parentFolder))
      //println(s"Resolved parent: $parentIri")

      val dataFormat = a.dataFormat.getOrElse(Shacl.defaultFormat)
      (a.data,a.schema) match {
        case (None,None) => {
          info(s"No data in manifestAction $a")
          numErrors += 1
          Right((Schema.empty, RDFAsJenaModel.empty))
        }
        case (Some(dataIri), Some(shapesIri)) => {
          val realDataIri = if (dataIri.isEmpty) {
            parentIri.resolve(IRI(fileName))
          } else {
            parentIri.resolve(dataIri)
          }
          val realSchemaIri = if (shapesIri.isEmpty) {
            parentIri.resolve(IRI(fileName))
          } else {
            parentIri.resolve(shapesIri)
          }
          for {
            rdf <- RDFAsJenaModel.fromURI(realDataIri.str, dataFormat)
            schemaRdf <- RDFAsJenaModel.fromURI(realSchemaIri.str, dataFormat)
            schema <- {
              RDF2Shacl.getShacl(schemaRdf)
            }
          } yield (schema, rdf)
        }
        case (None, Some(shapesIri)) => {
          val realSchemaIri = if (shapesIri.isEmpty) {
            absoluteIri + fileName
          } else {
            absoluteIri.resolve(shapesIri)
          }
          for {
            schemaRdf <- RDFAsJenaModel.fromURI(realSchemaIri.str, dataFormat)
            schema <- {
              RDF2Shacl.getShacl(schemaRdf)
            }
          } yield (schema, RDFAsJenaModel.empty)
        }
        case (Some(dataIri),None) => {
          val realDataIri = if (dataIri.isEmpty) {
            absoluteIri + fileName
          } else {
            absoluteIri.resolve(dataIri)
          }
          for {
            rdf <- RDFAsJenaModel.fromURI(realDataIri.str, dataFormat)
            schema <- {
              RDF2Shacl.getShacl(rdf)
            }
          } yield (schema, rdf)
        }
      }
    }

    def validate(schema: Schema, rdf: RDFReader, expectedResult: ManifestResult, name: String, parentFolder: String, node: RDFNode): Unit = {
      // info(s"Schema: ${schema.serialize("TURTLE", RDFAsJenaModel.empty)}")
      // info(s"RDF: ${rdf.serialize("TURTLE")}")
      val validator = Validator(schema)
      val result = validator.validateAll(rdf)
      val parentFolderURI = new java.net.URI(parentFolder)
      val testUriResolved = parentFolderURI.resolve(node.getLexicalForm)
      val testUri = (new java.net.URI("urn:x-shacl-test:/" + testUriResolved)).toString
      info(s"TestURI: $testUri, resolved: $testUriResolved, parent: $parentFolderURI")
      expectedResult match {
        case ReportResult(rep) => {
          rep.failingNodesShapes.foreach { case (node,shape) => {
            numTests += 1
            result.result.fold(vr => {
              numErrors += 1
              fail(s"Validating error: ${vr}")
              },
              typing => {
              // info(s"Checking that $node fails for shape $shape")
              // info(s"Typing: ${typing.show}")
              if (typing._1.getFailedValues(node).map(_.id) contains (shape)) {
                numPassed += 1
                val item = SingleTestReport(
                  passed = true,
                  name = name,
                  uriTest = testUri,
                  testType = "Validation",
                  moreInfo = s"$node failed as expected for shape $shape")
                report.addTestReport(item)
              } else {
                numFailed += 1
                val item = SingleTestReport(
                  passed = false,
                  name = name,
                  uriTest = testUri,
                  testType = "Validation",
                  moreInfo = s"$node did not fail for shape $shape")
                report.addTestReport(item)
              }
            })
           }
          }
        }
        case BooleanResult(b) =>
          if (result.isOK == b) {
            numTests += 1
            numPassed += 1
            val item = SingleTestReport(
              passed = true,
              name = name,
              uriTest = testUri,
              testType = "Validation",
              moreInfo = s"Expected $b and found it")
            report.addTestReport(item)
            info(s"Expected result = obtainedResult") // = $b.\nResult:\n${result}")
          }
          else {
            numTests += 1
            numFailed += 1
            val item = SingleTestReport(
              passed = false,
              name = name,
              uriTest = testUri,
              testType = "Validation",
              moreInfo = s"Expected $b and found ${!b}")
            report.addTestReport(item)
            fail(s"Expected result($b)!= obtained result\n$result")
          }
        case _ => fail(s"Unsupported manifestTest result $result")
      }
    }


}