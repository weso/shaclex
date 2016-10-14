package es.weso.rdf.nodes

import java.net.{ URISyntaxException, URI }

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
  def apply(str: String): IRI = {
    // Todo: Capture exceptions to provide better error messages?
    IRI(new URI(str))
  }

  def unapply(str: String): Option[IRI] = {
    try {
      Some(IRI(new URI(str)))
    } catch {
      case _: URISyntaxException => None
    }
  }

  def fromString(str: String): Option[IRI] = {
    unapply(str)
  }

}
