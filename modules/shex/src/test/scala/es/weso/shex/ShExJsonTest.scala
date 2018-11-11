package es.weso.shex
import org.scalatest._
import com.typesafe.config._
import java.io.File
import scala.io._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import es.weso.json._
import es.weso.utils.FileUtils._

class ShExJsonTest extends FunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List("coverage", "representationTests")

  def getJsonFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "json", ignoreFiles)
  }

  describe("Parsing Schemas from Json") {
    for (file <- getJsonFiles(schemasFolder)) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        shouldDecodeEncodeEqual[Schema](str)
      }
    }
  }
}
