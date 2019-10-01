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
        logger.debug(s"Alias: '$alias', localName: '$localname'")
        getIRI(alias).map(iri => iri.add(localname))
      }
    }
  }

  def contains(prefix: String): Boolean = pm.contains(Prefix(prefix))

  def addPrefix(prefix: String, iri: IRI): PrefixMap = {
    PrefixMap(pm + (Prefix(prefix) -> iri))
  }

  def addPrefixMap(other: PrefixMap): PrefixMap = {
    def cmb(current: PrefixMap, pair: (Prefix,IRI)): PrefixMap = {
      val (prefix,iri) = pair
      current.addPrefix(prefix,iri)
    }
    other.pm.foldLeft(this)(cmb)
  }


  def addPrefix(prefix: Prefix, iri: IRI): PrefixMap = {
    PrefixMap(pm + (prefix -> iri))
  }

  override def toString: String = {
    pm.map {
      case (prefix,iri) => "prefix " + prefix.str + ": " + iri.toString
    } .mkString("\n")
  }

  def qualifyIRI(iri: IRI): String = {
    getPrefixLocalName(iri) match {
      case Left(_) => iri.toString
      case Right((prefix,_,localName)) => prefix.str + ":" + localName
    }
  }

  def qualify(node: RDFNode): String =
    node match {
      case iri: IRI => qualifyIRI(iri)
      case _ => node.toString
    }

  /**
  * Get prefix declaration and local name of an IRI
    * @param iri
    * @return If there is a prefix declaration a: -> http://example.org/
    *     Then, getPrefixLocalName(IRI(http://example.org/foo") returns Right("a", "foo")
    */
  def getPrefixLocalName(iri: IRI): Either[String,(Prefix, IRI, String)] = {
    val str = iri.str
    val cs = pm.collect{
       case p if startsWithPredicate(str)(p) => {
         val (prefix, i) = p
         (prefix, i, str.stripPrefix(i.str))
       }
     }.toSeq
    cs.sortBy {
        case (_, _, str) => str.length
      }.
      headOption.
      toRight(s"Not found IRI starting by $iri in prefix map\n$this")
  }

  private def startsWithPredicate(str:String)(p: (Prefix, IRI)): Boolean = {
    str.startsWith(p._2.str)
  }

  /**
   * If prefixMap contains a: -> http://example.org/
   * then qualifyString("http://example.org/x") = "a:x"
   * else <http://example.org/x>
   */
  /* def qualifyString(str: String): String = {
    // TODO: Refactor this method to have IRI as parameter and reuse getPrefixLocalName
    val found = pm.collect{ case p if startsWithPredicate(str)(p) => (p, str.stripPrefix(p._2.str)) }.toSeq.sortBy(_._2.length)
    found.headOption match {
      case None => "<" ++ str ++ ">"
      case Some((p,localName)) => {
        if (localName contains ("/")) {
          "<" ++ str ++ ">"
        } else {
          p._1.str + ":" + localName
        }
      }
    }
  } */

  def prefixes: List[String] = {
    pm.keySet.map(_.str).toList
  }

  def merge(other: PrefixMap): PrefixMap = {
    val zero = this.pm
    def cmb(next: Map[Prefix,IRI], current: (Prefix,IRI)): Map[Prefix,IRI] = {
      val (prefix,iri) = current
      next.updated(prefix,iri)
    }
    PrefixMap(other.pm.foldLeft(zero)(cmb))
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

  def fromMap(pm: Map[String,IRI]): PrefixMap = {
    def cmb(pm: PrefixMap, current: (String,IRI)): PrefixMap =
      addPrefix(current._1,current._2)(pm)
    pm.foldLeft(empty)(cmb)
  }
}

