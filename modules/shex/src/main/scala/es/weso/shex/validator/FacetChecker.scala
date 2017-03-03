package es.weso.shex.validator

import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.PREFIXES._
import es.weso.rdf.nodes._
import es.weso.shex.ViolationError._
import es.weso.shex._
import es.weso.shex.validator.ShExChecker._
import es.weso.utils.RegEx

case class FacetChecker(schema: Schema)
  extends ShowValidator(schema) with LazyLogging {

  def checkFacets(attempt: Attempt,
                  node: RDFNode)(facets: List[XsFacet]): CheckTyping = for {
    ts <- checkAll(facets.map(checkFacet(attempt,node)(_)))
    t <- combineTypings(ts)
  } yield t

  def length(node: RDFNode): Int = node.getLexicalForm.length

  def checkFacet(attempt: Attempt,
                  node: RDFNode)(facet: XsFacet): CheckTyping = {
    logger.info(s"checkFacet: ${node.show} ${facet}")
    facet match {
      case Length(n) => {
        val l = length(node)
        checkCond(l == n,
                  attempt,
                  msgErr(s"${node.show} doesn't satisfy facet Length($n) with length $l"),
                  s"${node.show} satisfies Length($n) with length $l")
      }
      case MinLength(n) => {
        val l = length(node)
        checkCond(length(node) >= n,
          attempt,
          msgErr(s"${node.show} doesn't satisfy facet MinLength($n) with length $l"),
          s"${node.show} satisfies MinLength($n) with length $l")
      }
      case MaxLength(n) => {
        val l = length(node)
        checkCond(length(node) <= n,
          attempt,
          msgErr(s"${node.show} doesn't satisfy facet MaxLength($n) with length $l"),
          s"${node.show} satisfies MaxLength($n) with length $l")
      }
      case Pattern(p,flags) => {
        val str = node.getLexicalForm
        RegEx(p,flags).matches(str) match {
          case Right(b) => checkCond(b,attempt,
            msgErr(s"${node.show} doesn't match Pattern($p) with lexical form $str"),
            s"${node.show} satisfies Pattern($p) with lexical form $str")
          case Left(msg) => errStr(msg)
        }
      }
      case _ => {
        logger.error(s"Not implemented checkFacet: $facet")
        errStr(s"Not implemented checkFacet: $facet")
      }
    }
  }


  def matchRegex(pattern: String, str: String) = {

  }
}