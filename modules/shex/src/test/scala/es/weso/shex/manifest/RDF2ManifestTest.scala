package es.weso.shex.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest._

class RDF2ManifestTest extends FunSpec with ValidateManifest {

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("localFolderTest")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri.toString

  describe("RDF2Manifest") {
    parseManifest("manifest","extends",shexFolder)
  }
}
