package es.weso.json
import io.circe._
import org.scalatest._
import cats._, data._
import cats.implicits._
import io.circe.syntax._
import io.circe.parser._
import JsonDiff._

trait JsonTest extends FunSpec {
  
  def shouldDecodeEncodeEqual[A: Encoder: Decoder: Show](str: String): Unit = {
    parse(str) match {
      case Xor.Left(e) => fail(s"Cannot obtain Json from string. Error $e\nContents:\n$str")
      case Xor.Right(json) => {
        json.as[A] match {
          case Xor.Left(e) => fail(s"Cannot obtain Schema from Json. Error $e\nJson:\n${json.show}")
          case Xor.Right(v) => {
            val jsonEncoded = v.asJson
            if (json.equals(jsonEncoded)) {
              ()
            } else {
              fail(s"Jsons are different. Diff=${diff(json, jsonEncoded)}\nJson:\n${json.show}\nEncoded:${jsonEncoded.show}\nSchema:${v.show}")
            }
          }
        }
      }
    }
  }

}