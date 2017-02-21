package es.weso.shex
import org.scalatest._
import com.typesafe.config._
import java.io.File
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import util._
import scala.io._
import es.weso.shex._
import es.weso.shex.implicits._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import cats._, data._
import cats.implicits._
import es.weso.json._

class shexJsonSingle extends FunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  def getJsonFile(schemasDir: String, name: String): File = {
    getFileFromFolderWithExt(schemasDir, name, "json")
  }

  def getFileFromFolderWithExt(path: String, name: String, ext: String): File = {
    new File(schemasFolder + "/" + name + ".json")
  }

  describe("Parsing Schema from Json") {
    val name = "2OneInclude1"
    val file = getJsonFile(schemasFolder, name)
    it(s"Should read Schema from file ${file.getName}") {
      val str = Source.fromFile(file)("UTF-8").mkString
      shouldDecodeEncodeEqual[Schema](str)
    }
  }
}