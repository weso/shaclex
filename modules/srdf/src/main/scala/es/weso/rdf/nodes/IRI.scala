package es.weso.rdf.nodes

import java.net.{ URI, URISyntaxException }
import scala.util.Try
import scala.util.matching.Regex
import scala.util.Either._

case class IRI(uri: URI) extends RDFNode {

  def add(str: String): IRI = {
    IRI(uri.toString + str)
  }

  def +(str: String): IRI = {
    add(str)
  }

  override def toString = {
    "<" + uri.toString + ">"
  }

  implicit def minOrd = new Ordering[IRI] {
    def compare(a: IRI, b: IRI) = a.uri.compareTo(b.uri)
  }

  def str: String = {
    uri.toString
  }

  /**
   * Resolve an IRI against this IRI (which is taken as the base)
   * Currently, we employ java.net.URI algorithm to resolve
   */
  def resolve(iri: IRI): IRI = {
    IRI(uri.resolve(iri.uri))
  }

  override def getLexicalForm: String = {
    str
  }

}

object IRI {
  /**
   * Unsafe can raise an exception if the URI is not well formed
   * @param str
   * @return
   */
  def apply(str: String): IRI = {
    IRI(new URI(str))
  }

  def mkIRI(str: String): Try[IRI] =
    Try(IRI(new URI(str)))

  def unapply(str: String): Option[IRI] =
    mkIRI(str).toOption

  def fromString(str: String): Option[IRI] = {
    unapply(str)
  }

  lazy val iriRegex: Regex = "^(.*)$".r

  def parseIRI(str: String): Either[String, IRI] =
    str match {
      case iriRegex(i) => // TODO: Substitute by IRI.fromString(i)
        try {
          Right(IRI(i))
        } catch {
          case e: URISyntaxException =>
            Left(s"Error trying to parse IRI: $e, '$str'")
        }
      case _ =>
        Left(s"$str doesn't match IRI regex $iriRegex")
    }

}
