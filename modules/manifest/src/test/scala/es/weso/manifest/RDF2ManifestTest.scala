package es.weso.manifest

import com.typesafe.config.{ Config, ConfigFactory }
import org.scalatest._
import util._
import sext._
import java.nio.file.Paths

class RDF2ManifestTest extends FunSpec with Matchers with TryValues with OptionValues {

  describe("RDF2Manifest") {

    val conf: Config = ConfigFactory.load()
    val shaclFolder = conf.getString("shaclCore")
    val fileName = shaclFolder + "/manifest.ttl"
    val shaclFolderURI = Paths.get(shaclFolder).normalize.toUri.toString

    it("Read example manifest") {
      RDF2Manifest.read(fileName, "TURTLE", Some(shaclFolderURI)) match {
        case Failure(e) =>
          fail(s"Error reading $fileName\n$e")
        case Success(mf) =>
          info(s"Manifest successfully read. ${mf.treeString}")
      }
    }
  }
}
