package es.weso.shextest.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}

class RDF2ManifestSingleTest extends ValidateManifest {

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("localFolderTest")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri.toString

  describe("RDF2Manifest") {
     parseManifest("manifest", "basic", shexFolder)
  }

}
