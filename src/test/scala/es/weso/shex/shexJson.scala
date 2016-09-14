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



class shexJson extends FunSpec with Matchers with EitherValues {

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
        decode[Schema](str) match {
          case Xor.Right(schema) => info(s"Parsed ${schema.toString}")
          case Xor.Left(e) => fail(s"Error $e. Contents:\n$str")
        }
      }
    }
  }
}
 