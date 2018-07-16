package es.weso.rdf.operations

import java.sql.Time
import java.time.Instant

import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._

object Comparisons {

  sealed trait PrimitiveLiteral

  case class Datetime(dt: Instant) extends PrimitiveLiteral

  sealed trait NumericLiteral
  case class NumericInt(n: Int) extends NumericLiteral
  case class NumericDouble(n: Double, repr: String) extends NumericLiteral
  case class NumericDecimal(n: BigDecimal, repr: String) extends NumericLiteral

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

  def numericValue(node: RDFNode): Either[String, NumericLiteral] = node match {
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
    case DatatypeLiteral(str, other) => Left(s"Cannot convert to numeric value datatype literal $str^^$other")
    case _ => Left(s"Cannot convert $node to numeric literal for comparison")
  }

  def lessThanOrEquals(nl1: NumericLiteral, nl2: NumericLiteral): Boolean = (nl1,nl2) match {
    case (NumericInt(n1), NumericInt(n2)) => n1 <= n2
    case (NumericInt(n1), NumericDouble(n2,_)) => n1 <= n2
    case (NumericInt(n1), NumericDecimal(n2,_)) => n1 <= n2
    case (NumericDouble(n1,_), NumericInt(n2)) => n1 <= n2
    case (NumericDouble(n1,_), NumericDouble(n2,_)) => n1 <= n2
    case (NumericDouble(n1,_), NumericDecimal(n2,_)) => n1 <= n2
    case (NumericDecimal(n1,_), NumericInt(n2)) => n1 <= n2
    case (NumericDecimal(n1,_), NumericDouble(n2,_)) => n1 <= n2
    case (NumericDecimal(n1,_), NumericDecimal(n2,_)) => n1 <= n2
  }

  def lessThan(nl1: NumericLiteral, nl2: NumericLiteral): Boolean = (nl1,nl2) match {
    case (NumericInt(n1), NumericInt(n2)) => n1 < n2
    case (NumericInt(n1), NumericDouble(n2,_)) => n1 < n2
    case (NumericInt(n1), NumericDecimal(n2,_)) => n1 < n2
    case (NumericDouble(n1,_), NumericInt(n2)) => n1 < n2
    case (NumericDouble(n1,_), NumericDouble(n2,_)) => n1 < n2
    case (NumericDouble(n1,_), NumericDecimal(n2,_)) => n1 < n2
    case (NumericDecimal(n1,_), NumericInt(n2)) => n1 < n2
    case (NumericDecimal(n1,_), NumericDouble(n2,_)) => n1 < n2
    case (NumericDecimal(n1,_), NumericDecimal(n2,_)) => n1 < n2
  }

  def greaterThan(nl1: NumericLiteral, nl2: NumericLiteral): Boolean = lessThan(nl2,nl1)
  def greaterThanOrEquals(nl1:NumericLiteral,nl2: NumericLiteral): Boolean = lessThanOrEquals(nl2,nl1)


  def lessThanOrEquals(node1: RDFNode, node2: RDFNode): Either[String,Boolean] = for {
    nl1 <- numericValue(node1)
    nl2 <- numericValue(node2)
  } yield lessThanOrEquals(nl1,nl2)

  def lessThan(node1: RDFNode, node2: RDFNode): Either[String,Boolean] = for {
    nl1 <- numericValue(node1)
    nl2 <- numericValue(node2)
  } yield lessThan(nl1,nl2)

  def greaterThanOrEquals(node1:RDFNode, node2: RDFNode): Either[String,Boolean] = lessThanOrEquals(node2,node1)
  def greaterThan(node1:RDFNode, node2: RDFNode): Either[String,Boolean] = lessThan(node2,node1)

}