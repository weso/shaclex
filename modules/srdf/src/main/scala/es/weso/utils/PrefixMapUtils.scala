package es.weso.utils

import es.weso.rdf.nodes._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import es.weso.rdf.triples._

object PrefixMapUtils {

  def showTriples(g: Set[RDFTriple])(implicit pm: PrefixMap): String = {
    "{ " + g.map(x => showTriple(x)(pm)).mkString(". ") + " }"
  }

  def showTriple(t: RDFTriple)(implicit pm: PrefixMap): String = {
    showRDFNode(t.subj)(pm) + ", " + showIRI(t.pred) + ", " + showRDFNode(t.obj)(pm)
  }

  def showIRI(i: IRI)(implicit pm: PrefixMap): String = {
    pm.qualify(i)
  }

  def showRDFNode(n: RDFNode)(pm: PrefixMap): String = {
    n match {
      case i: IRI => showIRI(i)(pm)
      case b: BNode => b.toString
      case s: StringLiteral => escapeLexicalForm(s.lexicalForm)
      case s: DatatypeLiteral => escapeLexicalForm(s.lexicalForm) + "^^" + s.dataType.toString
      case l: Literal => l.toString

    }
  }

  def escapeLexicalForm(lex: String): String = {
    if (lex.contains("\"\'\\")) {
      tripleQuote + lex + tripleQuote
    } else "\"" + lex + "\""
  }

  lazy val tripleQuote = "\"\"\""

}
