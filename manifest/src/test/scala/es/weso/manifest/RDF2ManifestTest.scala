package es.weso.manifest
import org.scalatest._
import util._
import sext._

class RDF2ManifestTest extends
  FunSpec with Matchers with TryValues with OptionValues {

  describe("RDF2Manifest") {
    it("Read example manifest") {
      val fileName = "manifest/src/test/resources/manifest1.ttl"
      RDF2Manifest.read(fileName,"") match {
        case Failure(e) => fail(s"Error reading $fileName: $e")
        case Success(mf) => info(s"Manifest successfully read. ${mf.treeString}")
      }
    }
  }
}
