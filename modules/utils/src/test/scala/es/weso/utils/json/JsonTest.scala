package es.weso.utils.json
import io.circe._
import org.scalatest._
import cats._
import cats.implicits._
import io.circe.syntax._
import io.circe.parser._
import JsonCompare._

trait JsonTest extends FunSpec {

 def decodeJsonSchemaEncodeEquals[A: Encoder: Decoder: Show](str: String): Unit = {
    for {
      json   <- parse(str).leftMap(e => s"Error parsing $str: $e")
      schema <- json.as[A].leftMap(e => s"Error obtainning Schema from Json: $e\nJson:\n${json.show}")
      jsonEncoded = schema.asJson
      check <- if (json.equals(jsonEncoded)) Right(())
      else
        Left(
          s"Jsons and different: Diff=${jsonDiff(json, jsonEncoded)}\nJson:\n${json.show}\nEncoded:\n${jsonEncoded.show}\nSchema:${schema.show}")
    } yield check
  }.fold(e => fail(e), s => {})


}