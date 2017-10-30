package es.weso.rdf

import es.weso.rdf.nodes._

import scala.collection.immutable.Map
import com.typesafe.scalalogging.LazyLogging

/**
 * PrefixMap representation
 */
case class PrefixMap(pm: Map[Prefix, IRI]) extends LazyLogging {

  def isEmpty: Boolean = pm.isEmpty

  /**
   * Given an alias, return the IRI associated with the alias
   */
  def getIRI(prefix: String): Option[IRI] = {
    pm.get(Prefix(prefix))
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
        val (alias, colonLocalname) = str.splitAt(n)
        val localname = colonLocalname.drop(1)
        println(s"Alias: '$alias', localName: '$localname'")
        getIRI(alias).map(iri => iri.add(localname))
      }
    }
  }

  def contains(prefix: String): Boolean = pm.contains(Prefix(prefix))

  def addPrefix(prefix: String, iri: IRI): PrefixMap = {
    PrefixMap(pm + (Prefix(prefix) -> iri))
  }

  def addPrefix(prefix: Prefix, iri: IRI): PrefixMap = {
    PrefixMap(pm + (prefix -> iri))
  }

  override def toString: String = {
    def cnv(pair: (Prefix, IRI)): String = {
      pair._1.str + ": " + pair._2 + "|"
    }
    pm.map(cnv).mkString("\n")
  }

  def qualifyIRI(iri: IRI): String =
    qualifyString(iri.str)

  def qualify(node: RDFNode): String =
    node match {
      case iri: IRI => qualifyIRI(iri)
      case _ => node.toString
    }

  /**
   * If prefixMap contains a: -> http://example.org/
   * then qualifyString("http://example.org/x") = "a:x"
   * else <http://example.org/x>
   */
  def qualifyString(str: String): String = {
    def startsWithPredicate(p: (Prefix, IRI)): Boolean = {
      str.startsWith(p._2.str)
    }

    pm.find(startsWithPredicate) match {
      case None => "<" ++ str ++ ">"
      case Some(p) => p._1.str + ":" + str.stripPrefix(p._2.str)
    }
  }

  def prefixes: List[String] = {
    pm.keySet.map(_.str).toList
  }

}

object PrefixMap {
  def empty = PrefixMap(Map[Prefix, IRI]())

  def addPrefix(prefix: String, iri: IRI)(pm: PrefixMap): PrefixMap =
    pm.addPrefix(prefix, iri)

  def qualify(iri: IRI, pm: PrefixMap): String =
    pm.qualifyIRI(iri)

  def qualify(node: RDFNode, pm: PrefixMap): String =
    pm.qualify(node)

}

