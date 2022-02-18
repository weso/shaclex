package es.weso.shaclex
import cats.effect.unsafe.implicits.global
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.LazyLogging
import es.weso.shextest.manifest._

import java.net.URI
import java.nio.file.Paths

object ShExTestRunner extends LazyLogging {

  val conf: Config       = ConfigFactory.load()
  val shexFolder: String = conf.getString("validationFolder")
  val shexFolderURI: URI = Paths.get(shexFolder).normalize.toUri

  def run(testName: String): Unit = {
    val r =
      RDF2Manifest.read(Paths.get(shexFolder + "/" + "manifest.ttl"), "Turtle", Some(shexFolderURI.toString), false)
    r.attempt
      .unsafeRunSync()
      .fold(
        e => logger.error(s"Error reading manifest: $e"),
        mf => {
          logger.info(s"Manifest read with ${mf.entries.length} entries")
          for (e <- mf.entries) {
            if (testName == e.name) {
              logger.debug(s"Found test ${e.name}")
            }
          }
          logger.debug(s"End of search")
        }
      )

  }
}
