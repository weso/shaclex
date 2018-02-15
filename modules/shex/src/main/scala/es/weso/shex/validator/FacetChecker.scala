package es.weso.shex.validator
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

  /*  def matchRegex(pattern: String, str: String) = {

  } */

  // TODO: I'd like to refactor the following code to avoid DRY...
  // Problem, how to do it in a compatible way with type safety
  type Comparator = (NumericLiteral, RDFNode) => Check[Boolean]

  private def str2NumericInt(str: String): Either[String, NumericInt] = try {
    val n: Int = Integer.parseInt(str)
    Right(NumericInt(n))
  } catch {
    case _: NumberFormatException => Left(s"Cannot obtain numeric value from node $str")
  }

  private def str2NumericDecimal(str: String): Either[String, NumericDecimal] = try {
    val n: BigDecimal = BigDecimal(str)
    Right(NumericDecimal(n,str))
  } catch {
    case _: NumberFormatException => Left(s"Cannot obtain numeric value from node $str")
  }

  private def str2NumericDouble(str: String): Either[String, NumericDouble] = try {
    val n: Double = str.toDouble
    Right(NumericDouble(n,str))
  } catch {
    case _: NumberFormatException => Left(s"Cannot obtain numeric value from node $str")
  }

  private def numericValue(node: RDFNode): Either[String, NumericLiteral] = node match {
    case IntegerLiteral(i) => Right(NumericInt(i))
    case DoubleLiteral(d) => Right(NumericDouble(d,d.toString))
    case DecimalLiteral(d) => Right(NumericDecimal(d,d.toString))
    case DatatypeLiteral(str, `xsd_byte`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_decimal`) => str2NumericDecimal(str)
    case DatatypeLiteral(str, `xsd_double`) => str2NumericDouble(str)
    case DatatypeLiteral(str, `xsd_int`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_integer`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_long`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_positiveInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_negativeInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_nonPositiveInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_nonNegativeInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_short`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_unsignedLong`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_unsignedInt`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_unsignedShort`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_unsignedByte`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd_float`) => str2NumericDouble(str)
  }

  def minInclusive: Comparator = (nl, node) => nl match {
    case NumericInt(ni) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(ni <= nodeInt)
      case Right(NumericDouble(d,_)) => ok(ni <= d)
      case Right(NumericDecimal(d,_)) => ok(ni <= d)
      case _ => errStr(s"Cannot compare minInclusive($ni) with node $node")
    }
    case NumericDouble(nd,_) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(nd <= nodeInt)
      case Right(NumericDouble(d,_)) => ok(nd <= d)
      case Right(NumericDecimal(d,_)) => ok(nd <= d)
      case _ => errStr(s"Cannot compare minInclusive($nd) with node $node")
    }
    case NumericDecimal(nd,_) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(nd <= nodeInt)
      case Right(NumericDouble(d,_)) => ok(nd <= d)
      case Right(NumericDecimal(d,_)) => ok(nd <= d)
      case _ => errStr(s"Cannot compare minInclusive($nd) with node $node")
    }
  }

  def minExclusive: Comparator = (nl, node) => nl match {
    case NumericInt(ni) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(ni < nodeInt)
      case Right(NumericDouble(d,_)) => ok(ni < d)
      case Right(NumericDecimal(d,_)) => ok(ni < d)
      case _ => errStr(s"Cannot compare minExclusive($ni) with node $node")
    }
    case NumericDouble(nd,_) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(nd < nodeInt)
      case Right(NumericDouble(d,_)) => ok(nd < d)
      case Right(NumericDecimal(d,_)) => ok(nd < d)
      case _ => errStr(s"Cannot compare minExclusive($nd) with node $node")
    }
    case NumericDecimal(nd,_) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(nd < nodeInt)
      case Right(NumericDouble(d,_)) => ok(nd < d)
      case Right(NumericDecimal(d,_)) => ok(nd < d)
      case _ => errStr(s"Cannot compare minExclusive($nd) with node $node")
    }
  }

  def maxInclusive: Comparator = (nl, node) => nl match {
    case NumericInt(ni) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(ni >= nodeInt)
      case Right(NumericDouble(d,_)) => ok(ni >= d)
      case Right(NumericDecimal(d,_)) => ok(ni >= d)
      case _ => errStr(s"Cannot compare maxInclusive($ni) with node $node")
    }
    case NumericDouble(nd,_) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(nd >= nodeInt)
      case Right(NumericDouble(d,_)) => ok(nd >= d)
      case Right(NumericDecimal(d,_)) => ok(nd >= d)
      case _ => errStr(s"Cannot compare maxInclusive($nd) with node $node")
    }
    case NumericDecimal(nd,_) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(nd >= nodeInt)
      case Right(NumericDouble(d,_)) => ok(nd >= d)
      case Right(NumericDecimal(d,_)) => ok(nd >= d)
      case _ => errStr(s"Cannot compare maxInclusive($nd) with node $node")
    }
  }

  def maxExclusive: Comparator = (nl, node) => nl match {
    case NumericInt(ni) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(ni > nodeInt)
      case Right(NumericDouble(d,_)) => ok(ni > d)
      case Right(NumericDecimal(d,_)) => ok(ni > d)
      case _ => errStr(s"Cannot compare maxExclusive($ni) with node $node")
    }
    case NumericDouble(nd,_) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(nd > nodeInt)
      case Right(NumericDouble(d,_)) => ok(nd > d)
      case Right(NumericDecimal(d,_)) => ok(nd > d)
      case _ => errStr(s"Cannot compare maxExclusive($nd) with node $node")
    }
    case NumericDecimal(nd,_) => numericValue(node) match {
      case Right(NumericInt(nodeInt)) => ok(nd > nodeInt)
      case Right(NumericDouble(d,_)) => ok(nd > d)
      case Right(NumericDecimal(d,_)) => ok(nd > d)
      case _ => errStr(s"Cannot compare maxExclusive($nd) with node $node")
    }
  }
}