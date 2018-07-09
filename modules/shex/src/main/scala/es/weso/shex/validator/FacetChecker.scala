package es.weso.shex.validator
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.PREFIXES._
import es.weso.rdf.nodes._
import es.weso.rdf.operations.Comparisons._
import es.weso.shex.ShExError._
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



  def checkFacet(
    attempt: Attempt,
    node: RDFNode)(facet: XsFacet): CheckTyping = {
    logger.info(s"checkFacet: ${node.show} ${facet}")
    facet match {
      case Length(n) => {
        val l = NodeInfo.length(node)
        checkCond(
          l == n,
          attempt,
          msgErr(s"${node.show} does not satisfy facet Length($n) with length $l"),
          s"${node.show} satisfies Length($n) with length $l")
      }
      case MinLength(n) => {
        val l = NodeInfo.length(node)
        checkCond(
          l >= n,
          attempt,
          msgErr(s"${node.show} does not satisfy facet MinLength($n) with length $l"),
          s"${node.show} satisfies MinLength($n) with length $l")
      }
      case MaxLength(n) => {
        val l = NodeInfo.length(node)
        checkCond(
          l <= n,
          attempt,
          msgErr(s"${node.show} does not satisfy facet MaxLength($n) with length $l"),
          s"${node.show} satisfies MaxLength($n) with length $l")
      }
      case Pattern(p, flags) => {
        val str = node.getLexicalForm
        RegEx(p, flags).matches(str) match {
          case Right(b) => checkCond(b, attempt,
            msgErr(s"${node.show} does not match Pattern($p) with lexical form $str"),
            s"${node.show} satisfies Pattern($p) with lexical form $str")
          case Left(msg) => errStr(msg)
        }
      }
      case MinInclusive(m) => for {
        d <- minInclusive(m, node)
        r <- checkCond(d, attempt, msgErr(s"${node.show} does not match MinInclusive($m) with $node"),
          s"${node.show} satisfies MinInclusive($m)")
      } yield r
      case MinExclusive(m) => for {
        d <- minExclusive(m, node)
        r <- checkCond(d, attempt, msgErr(s"${node.show} does not match MinExclusive($m) with $node"),
          s"${node.show} satisfies MinExclusive($m)")
      } yield r
      case MaxInclusive(m) => for {
        d <- maxInclusive(m, node)
        r <- checkCond(d, attempt, msgErr(s"${node.show} does not match MaxInclusive($m) with $node"),
          s"${node.show} satisfies MaxInclusive($m)")
      } yield r
      case MaxExclusive(m) => for {
        d <- maxExclusive(m, node)
        r <- checkCond(d, attempt, msgErr(s"${node.show} does not match MaxExclusive($m) with $node"),
          s"${node.show} satisfies MaxExclusive($m)")
      } yield r
      case FractionDigits(m) => {
        val fd = NodeInfo.fractionDigits(node)
        checkCond(fd <= m, attempt,
          msgErr(s"${node.show} does not match FractionDigits($m) with $node and fraction digits = $fd"),
          s"${node.show} satisfies FractionDigits($m) with fraction digits = $fd")
      }
      case TotalDigits(m) => {
        val td = NodeInfo.totalDigits(node)
        checkCond(td <= m, attempt,
          msgErr(s"${node.show} does not match TotalDigits($m) with $node and totalDigits = $td"),
          s"${node.show} satisfies TotalDigits($m) with total digits = $td")
      }
      case _ => {
        logger.error(s"Not implemented checkFacet: $facet")
        errStr(s"Not implemented checkFacet: $facet")
      }
    }
  }

  // TODO: I'd like to refactor the following code to avoid DRY...
  // Problem, how to do it in a compatible way with type safety
  type Comparator = (NumericLiteral, RDFNode) => Check[Boolean]

  def compare(fn: (NumericLiteral,RDFNode) => Either[String,Boolean]
             ): (NumericLiteral, RDFNode) => Check[Boolean] = (nl,node) => {
    fn(nl,node).fold(e => errStr(e), v => ok(v))
  }

  def minInclusive: Comparator = compare((nl, node) => for {
    nl2 <- numericValue(node)
  } yield lessThanOrEquals(nl,nl2))


  def minExclusive: Comparator = compare((nl, node) => for {
    nl2 <- numericValue(node)
  } yield lessThan(nl,nl2))

  def maxInclusive: Comparator = compare((nl, node) => for {
    nl2 <- numericValue(node)
  } yield lessThanOrEquals(nl2,nl))


  def maxExclusive: Comparator = compare((nl, node) => for {
    nl2 <- numericValue(node)
  } yield lessThan(nl2,nl))

}