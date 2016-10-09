package es.weso.shex.compact
import org.scalatest._
import com.typesafe.config._
import cats.implicits._
import es.weso.json._
import es.weso.utils.FileUtils._
import scala.io._
import java.io.File
import es.weso.shex.compact.Parser._
import es.weso.shex.compact.printer._
import es.weso.shex.implicits.eqShEx._
import cats._, data._
import implicits._

class CompactSyntaxLocalTest extends FunSpec with Matchers with EitherValues {

  val conf: Config = ConfigFactory.load()
  val shexLocalFolder = conf.getString("shexLocalFolder")

  lazy val ignoreFiles: List[String] = List()

  def getShExFiles(schemasDir: String): List[File] = {
    getFilesFromFolderWithExt(schemasDir, "shex", ignoreFiles)
  }

  describe("Parsing ShEx files") {
    for (file <- getShExFiles(shexLocalFolder)) {
      it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        checkParseDeparse(str)
      }
    }
  }

  def checkParseDeparse(str: String) = {
    import es.weso.shex.implicits.eqShEx.eq
    parseSchema(str) match {
      case Left(str) => fail(s"Parsing error: $str")
      case Right(schema) => {
        val newStr = print(schema)
        parseSchema(newStr) match {
          case Left(e) =>
            fail(s"$str\n-- parsed as schema:\n${newStr}, but previous string doesn't parse\nError: $e")
          case Right(newSchema) =>
            if (Eq.eqv(schema,newSchema))
              info("Both schemas are equal")
            else
              fail(s"Schema1:\n$schema\n--Schema2:\n$newSchema\n")
        }
      }
    }
  }
}
