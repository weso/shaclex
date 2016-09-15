package es.weso.shex
import org.scalatest._
import com.typesafe.config._
import java.io.File
import io.circe._
import io.circe.parser._
import util._
import scala.io._
import es.weso.shex.implicits._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.showShEx._
import cats._, data._
import cats.implicits._
import es.weso.json._

class shexJson extends FunSpec with JsonTest with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  val ignoreFiles = List("coverage")

 def getJsonFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "json", ignoreFiles)
 }

 def getFilesFromFolderWithExt(path: String, ext: String, ignoreFiles: List[String]): List[(File)] = {
    val d = new File(path)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter { file =>
        if (file.isFile) {
          val (name,extension) = splitExtension(file.getName)
          extension == ext && !ignoreFiles.contains(name)
        } else false
      }.toList
    } else {
      List[File]()
    }
  }

  def splitExtension(str: String): (String,String) = {
    val splits = str.split('.')
    (splits.init.mkString("."),splits.last)
  }

  describe("Parsing Schemas from Json") {
    for(file <- getJsonFiles(schemasFolder)) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        shouldDecodeEncodeEqual[Schema](str)
      }
    }
  }
}
