package es.weso.shex.validator
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes._
import es.weso.rdf.operations.Comparisons._
import es.weso.shex.ShExError._
import es.weso.shex._
import es.weso.shex.validator.ShExChecker._
import es.weso.utils.RegEx


case class FacetChecker(schema: Schema, rdf: RDFReader)
  extends ShowValidator(schema) with LazyLogging {

  def checkFacets(
    attempt: Attempt,
    node: RDFNode)(facets: List[XsFacet]): CheckTyping = for {
    ts <- checkAll(facets.map(checkFacet(attempt, node)(_)))
    t <- combineTypings(ts)
  } yield t


  def checkFacet(attempt: Attempt,
                 node: RDFNode)(facet: XsFacet): CheckTyping = {
     facetChecker(node, facet).fold(errStr(_), addEvidence(attempt.nodeShape,_))
  }

  def facetsChecker(node: RDFNode, facets: List[XsFacet]): Either[String,String] =
    facets.map(facetChecker(node,_)).sequence.map(_.mkString("\n"))

  def facetChecker(node: RDFNode, facet: XsFacet): Either[String,String] = {
    facet match {
      case Length(n) => {
        val l = NodeInfo.length(node)
        checkCond(l == n,s"${node.show} does not satisfy facet Length($n) with length $l",
          s"${node.show} satisfies Length($n) with length $l")
      }
      case MinLength(n) => {
        val l = NodeInfo.length(node)
        checkCond(
          l >= n,
          s"${node.show} does not satisfy facet MinLength($n) with length $l",
          s"${node.show} satisfies MinLength($n) with length $l")
      }
      case MaxLength(n) => {
        val l = NodeInfo.length(node)
        checkCond(
          l <= n,
          s"${node.show} does not satisfy facet MaxLength($n) with length $l",
          s"${node.show} satisfies MaxLength($n) with length $l")
      }
      case Pattern(p, flags) => {
        val str = node.getLexicalForm
        RegEx(p, flags).matches(str) match {
          case Right(b) => checkCond(b,
            s"${node.show} does not match Pattern($p) with lexical form $str",
            s"${node.show} satisfies Pattern($p) with lexical form $str")
          case Left(msg) => Left(msg)
        }
      }
      case MinInclusive(m) => for {
        d <- minInclusive(m, node)
        r <- checkCond(d, s"${node.show} does not match MinInclusive($m) with $node",
          s"${node.show} satisfies MinInclusive($m)")
      } yield r
      case MinExclusive(m) => for {
        d <- minExclusive(m, node)
        r <- checkCond(d,s"${node.show} does not match MinExclusive($m) with $node",
          s"${node.show} satisfies MinExclusive($m)")
      } yield r
      case MaxInclusive(m) => for {
        d <- maxInclusive(m, node)
        r <- checkCond(d, s"${node.show} does not match MaxInclusive($m) with $node",
          s"${node.show} satisfies MaxInclusive($m)")
      } yield r
      case MaxExclusive(m) => for {
        d <- maxExclusive(m, node)
        r <- checkCond(d, s"${node.show} does not match MaxExclusive($m) with $node",
          s"${node.show} satisfies MaxExclusive($m)")
      } yield r
      case FractionDigits(m) => {
        val maybeFd = NodeInfo.fractionDigits(node,rdf)
        for {
          b <- maybeFd.fold(e => Left(e),
            fd => checkCond(fd <= m,
              s"${node.show} does not match FractionDigits($m) with $node and fraction digits = $fd",
              s"${node.show} satisfies FractionDigits($m) with fraction digits = $fd")
          )
        } yield b
      }
      case TotalDigits(m) => {
        val maybeTd = NodeInfo.totalDigits(node,rdf)
        for {
          b <- maybeTd.fold(e => Left(e), td =>
            checkCond(td <= m, s"${node.show} does not match TotalDigits($m) with $node and totalDigits = $td",
              s"${node.show} satisfies TotalDigits($m) with total digits = $td")
          )
        } yield b
      }
      case _ => s"Not implemented checkFacet: $facet".asLeft[String]
    }
  }

  // TODO: I'd like to refactor the following code to avoid DRY...
  // Problem, how to do it in a compatible way with type safety
  type Comparator = (NumericLiteral, RDFNode) => Either[String,Boolean]

  def compare(fn: (NumericLiteral,RDFNode) => Either[String,Boolean]
             ): (NumericLiteral, RDFNode) => Either[String,Boolean] =
    fn

  def minInclusive: Comparator = (nl, node) => for {
    nl2 <- numericValue(node)
  } yield lessThanOrEquals(nl,nl2)


  def minExclusive: Comparator = (nl, node) => for {
    nl2 <- numericValue(node)
  } yield lessThan(nl,nl2)

  def maxInclusive: Comparator = (nl, node) => for {
    nl2 <- numericValue(node)
  } yield lessThanOrEquals(nl2,nl)


  def maxExclusive: Comparator = (nl, node) => for {
    nl2 <- numericValue(node)
  } yield lessThan(nl2,nl)

  private def checkCond(cond: Boolean, msgTrue: String, msgFalse: String): Either[String, String] =
    if (cond) msgTrue.asRight[String]
    else msgFalse.asLeft[String]

}