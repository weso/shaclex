package es.weso.shaclex
import es.weso.shextest.manifest._
import com.typesafe.config.{Config, ConfigFactory}
import java.nio.file.Paths

object ShExTestRunner {

  val conf: Config = ConfigFactory.load()
  val shexFolder = conf.getString("validationFolder")
  val shexFolderURI = Paths.get(shexFolder).normalize.toUri

  def run(testName: String): Unit = {
    val r = RDF2Manifest.read(shexFolder + "/" + "manifest.ttl", "Turtle", Some(shexFolderURI.toString), false)
    r.fold(e => println(s"Error reading manifest: $e"),
      mf => {
        println(s"Manifest read with ${mf.entries.length} entries")
        for (e <- mf.entries) {
          if (testName == e.name) {
            println(s"Found test ${e.name}")
          }
        }
        println(s"End of search")
      })

  }
}