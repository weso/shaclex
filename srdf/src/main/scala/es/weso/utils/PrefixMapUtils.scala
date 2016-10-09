package es.weso.utils

import es.weso.rdf.nodes._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import es.weso.rdf.triples._

object PrefixMapUtils {
  
   private def iri2String(iri: IRI)(implicit pm: PrefixMap): String = {

    def startsWithPredicate(p: (String, IRI)): Boolean = {
      iri.str.startsWith(p._2.str)
    }

    pm.pm.find(startsWithPredicate) match {
      case None => "<" ++ iri.str ++ ">"
      case Some(p) => p._1 ++ ":" ++ iri.str.stripPrefix(p._2.str)
    }
  } 

  def qualify(iri: IRI)(implicit pm:PrefixMap): String = {
    iri2String(iri)(pm)
  }
    
    
  def showTriples(g: Set[RDFTriple])(implicit pm: PrefixMap): String = {
    "{ " + g.map(x => showTriple(x)(pm)).mkString(". ") + " }"
  }
  
  def showTriple(t: RDFTriple)(implicit pm: PrefixMap): String = {
    showRDFNode(t.subj)(pm) + ", " + showIRI(t.pred) + ", " + showRDFNode(t.obj)(pm)  
  }
  
  def showIRI(i: IRI)(implicit pm: PrefixMap): String = {
    qualify(i)(pm)
  }
  
  def showRDFNode(n: RDFNode)(pm: PrefixMap): String = {
    n match {
      case i: IRI => showIRI(i)(pm)
      case b: BNodeId => b.toString
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