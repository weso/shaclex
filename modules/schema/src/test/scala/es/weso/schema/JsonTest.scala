package es.weso.schema

import io.circe._
import org.scalatest._

trait JsonTest extends FunSpec with Matchers with EitherValues {

  def encodeDecodeTest[A](x: A)(implicit d: Decoder[A], e: Encoder[A]): Unit = {
    it(s"Encode/decode ${x.toString}") {
      val json = e.apply(x)
      val decoded = json.as[A]
      decoded.fold(failure => fail(s"Failure decoding $decoded: $failure"), value =>
        if (value == x)
          info(s"Encoded/decoded $x OK")
        else
          fail(s"Value $x != $value. Json = ${json.spaces2}"))
    }
  }
}
