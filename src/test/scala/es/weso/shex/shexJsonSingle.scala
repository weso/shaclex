package es.weso.shex
import org.scalatest._
import com.typesafe.config._
import java.io.File
import io.circe._
import io.circe.parser._
import util._
import scala.io._
import es.weso.shex.shexDecoder._
import es.weso.shex.shexShow._
import cats._, data._
import cats.implicits._

class shexJsonSingle extends FunSpec with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  def getJsonFile(schemasDir: String, name: String): File = {
    getFileFromFolderWithExt(schemasDir, name, "json")
  }

  def getFileFromFolderWithExt(path: String, name: String, ext: String): File = {
    new File(schemasFolder + "/" + name + ".json")
  }

  describe("Parsing Schema from Json") {
    val name = "open3groupdotcloseAnnot3"
    val file = getJsonFile(schemasFolder, name)
    it(s"Should read Schema from file ${file.getName}") {
      val str = Source.fromFile(file)("UTF-8").mkString
      decode[Schema](str) match {
        case Xor.Right(schema) => info(s"Parsed ${schema.toString}")
        case Xor.Left(e)       => fail(s"Error $e. Contents:\n$str")
      }
    }
  }
}
 