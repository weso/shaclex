package es.weso.utils

import scala.io.Source
import scala.util.Try
import cats.implicits._

object UriUtils {
  /**
    * Dereferentiate an URI
    * @param uri
    * @return Contents
    */
  def derefUri(uri: String): Either[String,String] = {
    Either.fromTry(Try(Source.fromURL(uri).mkString)).leftMap(_.getMessage)
  }

}