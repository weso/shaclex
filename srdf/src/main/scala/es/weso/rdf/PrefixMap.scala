package es.weso.rdf

import es.weso.rdf.nodes._
import scala.collection.immutable.Map

/**
 * PrefixMap for RDF
 */
case class PrefixMap(pm: Map[String, IRI]) {

  /**
   * Given an alias, return the IRI associated with the alias
   */
  def getIRI(prefix: String): Option[IRI] = {
    pm.get(prefix)
  }

  /**
   * Qualify a string 
   * If prefix map contains "ex" -> "http://example.org"
   * qname("ex:age") -> "http://example.org/age"
   */
  def qname(str: String): Option[IRI] = {
    str.indexOf(":") match {
      case (-1) => Some(IRI(str))
      case n => {
        val (alias, localname) = str.splitAt(n)
        getIRI(alias).map(iri => iri.add(localname))
      }
    }
  }

  def contains(prefix: String): Boolean = pm.contains(prefix)

  def addPrefix(prefix: String, iri: IRI): PrefixMap = {
    PrefixMap(pm + (prefix -> iri))
  }

  override def toString: String = {
    def cnv(pair: (String, IRI)): String = {
      pair._1 + ": " + pair._2 + "\n"
    }
    pm.map(cnv).mkString("\n")
  }

}

object PrefixMap {
  def empty = PrefixMap(Map[String, IRI]())

  def addPrefix(prefix: String, iri: IRI)(pm: PrefixMap): PrefixMap =
    pm.addPrefix(prefix, iri)

}

