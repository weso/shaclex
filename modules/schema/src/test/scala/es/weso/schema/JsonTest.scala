package es.weso.schema

import io.circe._
import munit._

trait JsonTest extends FunSuite {

  def encodeDecodeTest[A](x: A)(
    implicit 
     d: Decoder[A], 
     e: Encoder[A], 
     loc: munit.Location): Unit = {
    test(s"Encode/decode ${x.toString}") {
      val json = e.apply(x)
      val decoded = json.as[A]
      decoded.fold(failure => fail(s"Failure decoding $decoded: $failure"), value =>
        if (value == x)
          ()
        else
          fail(s"Value $x != $value. Json = ${json.spaces2}"))
    }
  }
}
