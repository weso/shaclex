package es.weso.utils

import java.net.URI

import scala.io.Source
import scala.util.Try
import cats.implicits._

object UriUtils {
  /**
    * Dereferentiate an URI
    * @param uri
    * @return Contents
    */
  def derefUri(uri: URI): Either[String,String] = {
    Either.fromTry(Try(Source.fromURI(uri).mkString)).leftMap(e => s"derefUri($uri): Error: ${e.getMessage}")
  }

}