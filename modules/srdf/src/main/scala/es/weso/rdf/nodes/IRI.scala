package es.weso.rdf.nodes

import cats.implicits._
import java.net.{URI, URISyntaxException}

import cats.Show

import scala.util.Try
import scala.util.matching.Regex

case class IRI(uri: URI) extends RDFNode {

  def add(str: String): IRI = {
    IRI(uri.toString + str)
  }

  def +(str: String): IRI = {
    add(str)
  }

  /**
    * Represents an IRI as <...>
    * @return
    */
  override def toString = {
    "<" + uri.parseServerAuthority.toString + ">"
  }

  implicit def minOrd = new Ordering[IRI] {
    def compare(a: IRI, b: IRI) = a.uri.compareTo(b.uri)
  }

  /**
    * String representation of IRI without < and >
    * @return string representation
    */
  def str: String = {
    uri.toString
  }

  /**
   * Resolve an IRI against this IRI (which is taken as the base)
   * Currently, we employ java.net.URI algorithm to resolve
   * It seems that the algorithm is wrong with file:// removing the two slashes
   */
  def resolve(iri: IRI): IRI = {
    IRI(uri.resolve(iri.uri))
  }

  override def getLexicalForm: String = {
    str
  }

  def isEmpty: Boolean = this == IRI("")

  def isEqualTo(other: RDFNode): Either[String,Boolean] = other match {
    case i: IRI => Right(i.uri == uri)
    case _ => Left(s"Type error comaring $this with $other")
  }

  def lessThan(other: RDFNode): Either[String,Boolean] = {
    other match {
      case otherIri: IRI => Right(uri.toString < otherIri.uri.toString)
      case _ => Left(s"Type error comaring $this with $other which is not an IRI")
    }
  }

  /**
  * Make a relative IRI from a given base
    *  Example `IRI("http://example.com/foo").relativize(IRI("http://example.com/")) = IRI("foo")``
    * @param base base IRI
    * @return the relativized IRI
    */
  def relativizeIRI(base: IRI): IRI =
    IRI(base.uri.relativize(uri))
}

object IRI {

/*  def apply(uri: URI): IRI =
    IRI(uri) */

  /**
   * Unsafe can raise an exception if the URI is not well formed
   * @param str
   * @return
   */
  def apply(str: String): IRI = {
    IRI(new URI(str))
  }

  def fromString(str: String, base: Option[IRI] = None): Either[String,IRI] = {
    Try{
      val uri = new URI(str)
      IRI(base.fold(uri)(_.uri.resolve(uri)))
    }.toEither.leftMap(_.getMessage)
  }

  def unapply(str: String): Option[IRI] =
    fromString(str).fold(_ => None, Some(_))

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

  implicit val iriShow = Show.show[IRI] { _.toString }

}
