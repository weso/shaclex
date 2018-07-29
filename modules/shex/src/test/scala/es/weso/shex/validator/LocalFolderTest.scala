package es.weso.shex.validator

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.rdf.jena._
import es.weso.shapeMaps.ShapeMap
import es.weso.shex._
import es.weso.utils.FileUtils.getFilesFromFolderWithExt
import org.scalatest._

import scala.util._

class LocalFolderTest extends FunSpec with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load
  val schemasFolder = conf.getString("localFolderTest")

  val ignoreFiles = List("coverage")

  def getJsonFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "json", ignoreFiles)
  }

  describe("Local folder test") {

  }
}
