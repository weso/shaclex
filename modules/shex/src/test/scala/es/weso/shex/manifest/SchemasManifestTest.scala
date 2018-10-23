package es.weso.shex.manifest

import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}

class SchemasManifestTest extends ValidateManifest {

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("schemasFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri.toString

  describe("RDF2ManifestLocal") {
    it(s"Should read manifest") {
    val r = RDF2Manifest.read(shexFolder + "/" + "manifest.ttl", "Turtle", None, false)
    r.fold(e => fail(s"Error reading manifest: $e"),
      mf => {
        for (e <- mf.entries) {
          info(s"Entry: $e")
        }
        info(s"Manifest read OK: ${mf.entries.length} entries")
      }
    )
   }
  }

}
