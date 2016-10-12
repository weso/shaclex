package es.weso.rdf

import es.weso.rdf.nodes._
import scala.collection.immutable.Map

/**
 * PrefixMap representation
 */
case class PrefixMap(pm: Map[Prefix, IRI]) {

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
        val (alias, localname) = str.splitAt(n)
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
      pair._1.str + ": " + pair._2 + "\n"
    }
    pm.map(cnv).mkString("\n")
  }

  /**
    * If prefixMap contains a: -> http://example.org/
    * then qualify(IRI("http://example.org/x")) = "a:x"
    * else <http://example.org/x>
    */
  def qualify(iri: IRI): String = {
      def startsWithPredicate(p: (Prefix, IRI)): Boolean = {
        iri.str.startsWith(p._2.str)
      }

      pm.find(startsWithPredicate) match {
        case None => "<" ++ iri.str ++ ">"
        case Some(p) => p._1.str + iri.str.stripPrefix(p._2.str)
      }
  }

}

object PrefixMap {
  def empty = PrefixMap(Map[Prefix, IRI]())

  def addPrefix(prefix: String, iri: IRI)(pm: PrefixMap): PrefixMap =
    pm.addPrefix(prefix, iri)

  def qualify(iri: IRI, pm: PrefixMap): String =
    pm.qualify(iri)

}

