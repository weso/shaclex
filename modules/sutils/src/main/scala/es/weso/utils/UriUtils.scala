package es.weso.utils

import java.net.URI

import cats.implicits._

import scala.io.Source
import scala.util.Try

object UriUtils {
  /**
    * Dereferentiate an URI
    * @param uri
    * @return Contents
    */
  def derefUri(uri: URI): Either[String,String] = {
    Either.fromTry(
      Try{
        val urlCon = uri.toURL.openConnection()
        urlCon.setConnectTimeout(4000)
        urlCon.setReadTimeout(2000)
        val is = urlCon.getInputStream()
        Source.fromInputStream(is).mkString
      }
    ).leftMap(e => s"derefUri($uri): Error: ${e.getMessage}")
  }

}