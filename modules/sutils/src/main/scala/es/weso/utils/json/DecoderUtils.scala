package es.weso.utils.json

import io.circe._
import cats.syntax.either._

object DecoderUtils {

  // Utils...

  def fixedFieldValue(c: HCursor, name: String, value: String): Decoder.Result[String] = {
    val str: Decoder.Result[String] = c.downField(name).as[String]
    str match {
      case Left(e) => Left(e)
      case Right(v) => if (v == value)
        Either.right(v)
      else
        Either.left(DecodingFailure(s"Required $value for field $name but got $v", Nil))
    }
  }

  def fieldDecode[A: Decoder](c: HCursor, name: String): Decoder.Result[A] =
    c.downField(name).as[A]

  def optFieldDecode[A: Decoder](c: HCursor, name: String): Decoder.Result[Option[A]] = {
    val x = c.downField(name)
    if (x.succeeded) {
      x.as[A] match {
        case Left(e) => Left(e)
        case Right(v) => Right(Some(v))
      }
    } else Either.right(None)
  }

  def optFieldDecodeMap[A: KeyDecoder, B: Decoder](c: HCursor, name: String): Decoder.Result[Option[Map[A, B]]] = {
    val x = c.downField(name)
    if (x.succeeded) {
      x.as[Map[A, B]] match {
        case Left(e) => Left(e)
        case Right(v) => Right(Some(v))
      }
    } else Either.right(None)
  }

  /**
   * Explicit mapDecoder which needs the key decoder as well as the value decoder
   * It should not be needed, but I found some problems about implicits resolution which I solved by
   * using this method
   */
  def mapDecoder[A, B](c: ACursor)(ka: KeyDecoder[A], db: Decoder[B]): Decoder.Result[Map[A, B]] = {
    implicit val ika = ka
    implicit val idb = db
    c.as[Map[A, B]]
  }

}