package es.weso.shex.validator

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.utils.FileUtils.getFilesFromFolderWithExt
import org.scalatest._

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
