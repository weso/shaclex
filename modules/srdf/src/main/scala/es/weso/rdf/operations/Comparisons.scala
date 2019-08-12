package es.weso.rdf.operations

import java.time.Instant

import cats._
import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._

object Comparisons {

  sealed trait PrimitiveLiteral

  case class Datetime(dt: Instant) extends PrimitiveLiteral

  sealed trait NumericLiteral
  case class NumericInt(n: Int, repr: String) extends NumericLiteral
  case class NumericDouble(n: Double, repr: String) extends NumericLiteral
  case class NumericDecimal(n: BigDecimal, repr: String) extends NumericLiteral

  private def str2NumericInt(str: String): Either[String, NumericInt] = try {
    val n: Int = Integer.parseInt(str)
    Right(NumericInt(n, str))
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
    case IntegerLiteral(i, repr) => Right(NumericInt(i,repr))
    case DoubleLiteral(d, repr) => Right(NumericDouble(d,repr))
    case DecimalLiteral(d, repr) => Right(NumericDecimal(d,repr))
    case DatatypeLiteral(str, `xsd:byte`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:decimal`) => str2NumericDecimal(str)
    case DatatypeLiteral(str, `xsd:double`) => str2NumericDouble(str)
    case DatatypeLiteral(str, `xsd:int`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:integer`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:long`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:positiveInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:negativeInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:nonPositiveInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:nonNegativeInteger`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:short`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:unsignedLong`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:unsignedInt`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:unsignedShort`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:unsignedByte`) => str2NumericInt(str)
    case DatatypeLiteral(str, `xsd:float`) => str2NumericDouble(str)
    case DatatypeLiteral(str, other) => Left(s"Cannot convert to numeric value datatype literal $str^^$other")
    case _ => Left(s"Cannot convert $node to numeric literal for comparison")
  }

  def lessThanOrEquals(nl1: NumericLiteral, nl2: NumericLiteral): Boolean = (nl1,nl2) match {
    case (NumericInt(n1,_), NumericInt(n2,_)) => n1 <= n2
    case (NumericInt(n1,_), NumericDouble(n2,_)) => n1 <= n2
    case (NumericInt(n1,_), NumericDecimal(n2,_)) => n1 <= n2
    case (NumericDouble(n1,_), NumericInt(n2,_)) => n1 <= n2
    case (NumericDouble(n1,_), NumericDouble(n2,_)) => n1 <= n2
    case (NumericDouble(n1,_), NumericDecimal(n2,_)) => n1 <= n2
    case (NumericDecimal(n1,_), NumericInt(n2,_)) => n1 <= n2
    case (NumericDecimal(n1,_), NumericDouble(n2,_)) => n1 <= n2
    case (NumericDecimal(n1,_), NumericDecimal(n2,_)) => n1 <= n2
  }

  def lessThan(nl1: NumericLiteral, nl2: NumericLiteral): Boolean = (nl1,nl2) match {
    case (NumericInt(n1,_), NumericInt(n2,_)) => n1 < n2
    case (NumericInt(n1,_), NumericDouble(n2,_)) => n1 < n2
    case (NumericInt(n1,_), NumericDecimal(n2,_)) => n1 < n2
    case (NumericDouble(n1,_), NumericInt(n2,_)) => n1 < n2
    case (NumericDouble(n1,_), NumericDouble(n2,_)) => n1 < n2
    case (NumericDouble(n1,_), NumericDecimal(n2,_)) => n1 < n2
    case (NumericDecimal(n1,_), NumericInt(n2,_)) => n1 < n2
    case (NumericDecimal(n1,_), NumericDouble(n2,_)) => n1 < n2
    case (NumericDecimal(n1,_), NumericDecimal(n2,_)) => n1 < n2
  }

  def greaterThan(nl1: NumericLiteral, nl2: NumericLiteral): Boolean = lessThan(nl2,nl1)
  def greaterThanOrEquals(nl1:NumericLiteral,nl2: NumericLiteral): Boolean = lessThanOrEquals(nl2,nl1)


  def lessThanOrEquals(node1: RDFNode, node2: RDFNode): Either[String,Boolean] = node1 lessThanOrEquals(node2)
  /*for {
    nl1 <- numericValue(node1)
    nl2 <- numericValue(node2)
  } yield lessThanOrEquals(nl1,nl2) */

  def lessThan(node1: RDFNode, node2: RDFNode): Either[String,Boolean] = node1 lessThan(node2)
  /*for {
    nl1 <- numericValue(node1)
    nl2 <- numericValue(node2)
  } yield lessThan(nl1,nl2)*/

  def greaterThanOrEquals(node1:RDFNode, node2: RDFNode): Either[String,Boolean] = lessThanOrEquals(node2,node1)
  def greaterThan(node1:RDFNode, node2: RDFNode): Either[String,Boolean] = lessThan(node2,node1)


  def contains[F[_]: Foldable](ns: F[RDFNode], node: RDFNode): Either[String,Boolean] = {
    Foldable[F].existsM(ns)(n => node.isEqualTo(n))
  }

  def notContained(ns: List[RDFNode], targets: List[RDFNode]): Either[String,List[RDFNode]] = {
    val zero: List[RDFNode] = List()
    def cmb(rest: List[RDFNode], a: RDFNode): Either[String,List[RDFNode]] =
      contains(targets,a).map(b => if (b) rest else (a :: rest))
    Foldable[List].foldM(ns,zero)(cmb)
  }

  def different(ns1: List[RDFNode], ns2: List[RDFNode]): Either[String,List[RDFNode]] = for {
    d1 <- notContained(ns1,ns2)
    d2 <- notContained(ns2,ns1)
  } yield (d1 union d2)

}
