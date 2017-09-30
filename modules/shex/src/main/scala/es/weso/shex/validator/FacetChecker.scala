package es.weso.shex.validator

import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.checking.Checker
import es.weso.rdf.PREFIXES._
import es.weso.rdf.nodes._
import es.weso.shex.ViolationError._
import es.weso.shex._
import es.weso.shex.validator.ShExChecker._
import es.weso.utils.RegEx

case class FacetChecker(schema: Schema)
  extends ShowValidator(schema) with LazyLogging {

  def checkFacets(
    attempt: Attempt,
    node: RDFNode)(facets: List[XsFacet]): CheckTyping = for {
    ts <- checkAll(facets.map(checkFacet(attempt, node)(_)))
    t <- combineTypings(ts)
  } yield t

  def length(node: RDFNode): Int = node.getLexicalForm.length

  def checkFacet(
    attempt: Attempt,
    node: RDFNode)(facet: XsFacet): CheckTyping = {
    logger.info(s"checkFacet: ${node.show} ${facet}")
    facet match {
      case Length(n) => {
        val l = length(node)
        checkCond(
          l == n,
          attempt,
          msgErr(s"${node.show} doesn't satisfy facet Length($n) with length $l"),
          s"${node.show} satisfies Length($n) with length $l")
      }
      case MinLength(n) => {
        val l = length(node)
        checkCond(
          length(node) >= n,
          attempt,
          msgErr(s"${node.show} doesn't satisfy facet MinLength($n) with length $l"),
          s"${node.show} satisfies MinLength($n) with length $l")
      }
      case MaxLength(n) => {
        val l = length(node)
        checkCond(
          length(node) <= n,
          attempt,
          msgErr(s"${node.show} doesn't satisfy facet MaxLength($n) with length $l"),
          s"${node.show} satisfies MaxLength($n) with length $l")
      }
      case Pattern(p, flags) => {
        val str = node.getLexicalForm
        RegEx(p, flags).matches(str) match {
          case Right(b) => checkCond(b, attempt,
            msgErr(s"${node.show} doesn't match Pattern($p) with lexical form $str"),
            s"${node.show} satisfies Pattern($p) with lexical form $str")
          case Left(msg) => errStr(msg)
        }
      }
      case MinInclusive(m) => for {
        d <- minInclusive(m,node)
        r <- checkCond(d, attempt, msgErr(s"${node.show} doesn't match MinInclusive($m) with $node"),
          s"${node.show} satisfies MinInclusive($m)")
      } yield r
      case MinExclusive(m) => for {
        d <- minExclusive(m,node)
        r <- checkCond(d, attempt, msgErr(s"${node.show} doesn't match MinExclusive($m) with $node"),
          s"${node.show} satisfies MinExclusive($m)")
      } yield r
      case MaxInclusive(m) => for {
        d <- maxInclusive(m,node)
        r <- checkCond(d, attempt, msgErr(s"${node.show} doesn't match MaxInclusive($m) with $node"),
          s"${node.show} satisfies MaxInclusive($m)")
      } yield r
      case MaxExclusive(m) => for {
        d <- maxExclusive(m,node)
        r <- checkCond(d, attempt, msgErr(s"${node.show} doesn't match MaxExclusive($m) with $node"),
          s"${node.show} satisfies MaxExclusive($m)")
      } yield r

      case _ => {
        logger.error(s"Not implemented checkFacet: $facet")
        errStr(s"Not implemented checkFacet: $facet")
      }
    }
  }

/*  def matchRegex(pattern: String, str: String) = {

  } */

  // TODO: I'd like to refactor the following code to avoid DRY...
  // Problem, how to do it in a compatible way with type safety
  type Comparator = (NumericLiteral, RDFNode) => Check[Boolean]

  def minInclusive: Comparator = (nl,node) => nl match {
    case NumericInt(ni) => node match {
      case IntegerLiteral(nodeInt) => ok(ni <= nodeInt)
      case DoubleLiteral(d) => ok(ni <= d)
      case DecimalLiteral(d) => ok(ni <= d)
      case _ => errStr(s"Cannot compare minInclusive($ni) with node $node")
    }
    case NumericDouble(nd) => node match {
      case IntegerLiteral(nodeInt) => ok(nd <= nodeInt)
      case DoubleLiteral(d) => ok(nd <= d)
      case DecimalLiteral(d) => ok(nd <= d)
      case _ => errStr(s"Cannot compare minInclusive($nd) with node $node")
    }
    case NumericDecimal(nd) => node match {
      case IntegerLiteral(nodeInt) => ok(nd <= nodeInt)
      case DoubleLiteral(d) => ok(nd <= d)
      case DecimalLiteral(d) => ok(nd <= d)
      case _ => errStr(s"Cannot compare minInclusive($nd) with node $node")
    }
  }

  def minExclusive: Comparator = (nl,node) => nl match {
    case NumericInt(ni) => node match {
      case IntegerLiteral(nodeInt) => ok(ni < nodeInt)
      case DoubleLiteral(d) => ok(ni < d)
      case DecimalLiteral(d) => ok(ni < d)
      case _ => errStr(s"Cannot compare minExclusive($ni) with node $node")
    }
    case NumericDouble(nd) => node match {
      case IntegerLiteral(nodeInt) => ok(nd < nodeInt)
      case DoubleLiteral(d) => ok(nd < d)
      case DecimalLiteral(d) => ok(nd < d)
      case _ => errStr(s"Cannot compare minExclusive($nd) with node $node")
    }
    case NumericDecimal(nd) => node match {
      case IntegerLiteral(nodeInt) => ok(nd < nodeInt)
      case DoubleLiteral(d) => ok(nd < d)
      case DecimalLiteral(d) => ok(nd < d)
      case _ => errStr(s"Cannot compare minExclusive($nd) with node $node")
    }
  }

  def maxInclusive: Comparator = (nl,node) => nl match {
    case NumericInt(ni) => node match {
      case IntegerLiteral(nodeInt) => ok(ni >= nodeInt)
      case DoubleLiteral(d) => ok(ni >= d)
      case DecimalLiteral(d) => ok(ni >= d)
      case _ => errStr(s"Cannot compare maxInclusive($ni) with node $node")
    }
    case NumericDouble(nd) => node match {
      case IntegerLiteral(nodeInt) => ok(nd >= nodeInt)
      case DoubleLiteral(d) => ok(nd >= d)
      case DecimalLiteral(d) => ok(nd >= d)
      case _ => errStr(s"Cannot compare maxInclusive($nd) with node $node")
    }
    case NumericDecimal(nd) => node match {
      case IntegerLiteral(nodeInt) => ok(nd >= nodeInt)
      case DoubleLiteral(d) => ok(nd >= d)
      case DecimalLiteral(d) => ok(nd >= d)
      case _ => errStr(s"Cannot compare maxInclusive($nd) with node $node")
    }
  }

  def maxExclusive: Comparator = (nl,node) => nl match {
    case NumericInt(ni) => node match {
      case IntegerLiteral(nodeInt) => ok(ni > nodeInt)
      case DoubleLiteral(d) => ok(ni > d)
      case DecimalLiteral(d) => ok(ni > d)
      case _ => errStr(s"Cannot compare maxExclusive($ni) with node $node")
    }
    case NumericDouble(nd) => node match {
      case IntegerLiteral(nodeInt) => ok(nd > nodeInt)
      case DoubleLiteral(d) => ok(nd > d)
      case DecimalLiteral(d) => ok(nd > d)
      case _ => errStr(s"Cannot compare maxExclusive($nd) with node $node")
    }
    case NumericDecimal(nd) => node match {
      case IntegerLiteral(nodeInt) => ok(nd > nodeInt)
      case DoubleLiteral(d) => ok(nd > d)
      case DecimalLiteral(d) => ok(nd > d)
      case _ => errStr(s"Cannot compare maxExclusive($nd) with node $node")
    }
  }

}