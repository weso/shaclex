package es.weso.manifest

import com.typesafe.config.{ Config, ConfigFactory }
import org.scalatest._
import util._
import sext._
import java.nio.file.Paths

class RDF2ManifestTest extends FunSpec with Matchers with TryValues with OptionValues {

  val conf: Config = ConfigFactory.load()
  val shaclFolder = conf.getString("shaclTests")
  val shaclFolderURI = Paths.get(shaclFolder).normalize.toUri.toString

  describe("RDF2Manifest") {
    parseManifest("personexample","core/complex")
    parseManifest("manifest", "core/complex")
    parseManifest("manifest", "core")
  }

  def parseManifest(name: String,folder: String): Unit = {
    it(s"Should parse manifest $folder/$name") {
      val fileName = s"$shaclFolder/$folder/$name.ttl"
      RDF2Manifest.read(fileName, "TURTLE", Some(s"$shaclFolderURI$folder/"), true) match {
        case Left(e) =>
          fail(s"Error reading $fileName\n$e")
        case Right(mf) =>
          info(s"Manifest successfully read. ${mf.treeString}")
      }
    }
  }
}
