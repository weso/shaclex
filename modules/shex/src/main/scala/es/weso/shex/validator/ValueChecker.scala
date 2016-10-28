package es.weso.shex.validator
import cats._
import com.typesafe.scalalogging.LazyLogging
import data._
import implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.shex.ViolationError._
import es.weso.shex._
import es.weso.shex.validator.ShExChecker._

case class ValueChecker(schema: Schema) extends ShowValidator(schema) with LazyLogging {

  def checkValue(attempt: Attempt,
                 node: RDFNode)(value: ValueSetValue): CheckTyping = {
    logger.info(s"checkValue: $node $value")
    value match {
      case IRIValue(iri) => node match {
        case i: IRI =>
          checkCond(iri == i,attempt,
            msgErr(s"${node.show} is not ${i.show}"),
            s"${node.show} == ${i.show}")
        case _ => errStr(s"${node.show} != ${iri.show}")
      }
/*      case StringValue(s) => node match {
        case DatatypeLiteral(str,d) => ok(d == xsd_string && s == str)
        case StringLiteral(str) => ok(s == str)
        case _ => ok(false)
       }
      case DatatypeString(s,iri) => node match {
        case l: Literal => ok(l.dataType == iri && l.getLexicalForm == s)
        case _ => ok(false)
      }
      case LangString(s,lang) => node match {
        case LangLiteral(str,l) => ok(s == str && l == lang)
        case _ => ok(false)
      }
      case Stem(stem) => errStr(s"Not implemented stem: $stem")
      case StemRange(stem,exclusions) => errStr(s"Not implemented stem range: $stem $exclusions") */

      case _ => errStr(s"Not implemented checkValue: $value")
    }
  }
}