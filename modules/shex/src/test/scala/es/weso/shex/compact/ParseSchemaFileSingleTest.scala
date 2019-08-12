package es.weso.shex.compact

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import es.weso.utils.json.JsonTest
import es.weso.utils.FileUtils
import org.scalatest.{EitherValues, FunSpec, Matchers}

class ParseSchemaFileSingleTest extends FunSpec with JsonTest with Matchers with EitherValues {

  val name = "1focusLength-dot"
  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  describe(s"Parsing single File $name") {
    val file: File = FileUtils.getFileFromFolderWithExt(schemasFolder, name, "shex")
      it(s"Should read Schema from file ${file.getName()}") {
      val parsed = for {
        cs <- FileUtils.getContents(file)
        schema <- Parser.parseSchema(cs.toString,None)
      } yield schema
      parsed match {
        case Right(schema) => info(s"Parsed:\n$schema")
        case Left(err) => fail(s"Parsing error: $err")
      }
    }
  }

}
