package es.weso.shex.compact

import java.io.File

import cats._
import cats.data._
import com.typesafe.config.{Config, ConfigFactory}
import es.weso.json.JsonTest
import es.weso.shex._
import es.weso.shex.implicits.encoderShEx._
import es.weso.utils.FileUtils._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.{EitherValues, FunSpec, Matchers}

import scala.io._
import scala.util.{Failure, Success}

class CompareJsonSingle extends FunSpec with JsonTest with Matchers with EitherValues {

  val name = "startCode1"
  val conf: Config = ConfigFactory.load()
  val schemasFolder = conf.getString("schemasFolder")

  describe(s"Parsing single File $name") {
    val file : File = getFileFromFolderWithExt(schemasFolder,name,"shex")
    it(s"Should read Schema from file ${file.getName}") {
        val str = Source.fromFile(file)("UTF-8").mkString
        Schema.fromString(str,"SHEXC",None) match {
          case Success(schema) => {
            val (name,ext) = splitExtension(file.getName)
            val jsonFile = schemasFolder + "/"+ name + ".json"
            val jsonStr = Source.fromFile(jsonFile)("UTF-8").mkString
            parse(jsonStr) match {
              case Xor.Left(err) => fail(s"Error parsing $jsonFile: $err")
              case Xor.Right(json) =>
                if (json.equals(schema.asJson)) {
                 info("Jsons are equal")
                } else {
               fail(s"Json's are different. Parsed:\n${schema.asJson.spaces4}\n-----Expected:\n${json.spaces4}")
            }
          }}
          case Failure(err) => fail(s"Parsing error: $err")
        }
      }
    }
}
