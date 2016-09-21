package es.weso.shex
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._

case class Schema(
    prefixes: Option[Map[Prefix,IRI]],
    base: Option[IRI],
    startActs: Option[List[SemAct]],
    start: Option[ShapeExpr],
    shapes: Option[Map[ShapeLabel,ShapeExpr]]
) {

  def resolveShapeLabel(l: ShapeLabel): Either[String,IRI] = l match {
    case IRILabel(iri) => Right(iri)
    case _ => Left(s"Label $l can't be converted to IRI")
  }

  lazy val prefixMap: Map[Prefix,IRI] =
    prefixes.getOrElse(Map())

}

case class Prefix(s: String)

abstract sealed trait ShapeExpr

case class ShapeOr(shapeExprs: List[ShapeExpr]) extends ShapeExpr

case class ShapeAnd(shapeExprs: List[ShapeExpr]) extends ShapeExpr

case class ShapeNot(shapeExpr: ShapeExpr) extends ShapeExpr

case class NodeConstraint(
    nodeKind: Option[NodeKind],
    datatype: Option[IRI],
    xsFacets: List[XsFacet],
    values: Option[List[ValueSetValue]]
    ) extends ShapeExpr
object NodeConstraint {

  def empty = NodeConstraint(
          nodeKind = None,
          datatype = None,
          xsFacets = List(),
          values = None
      )

  def nodeKind(nk: NodeKind): NodeConstraint =
    NodeConstraint.empty.copy(nodeKind = Some(nk))

  def datatype(dt: IRI): NodeConstraint =
    NodeConstraint.empty.copy(datatype = Some(dt))

  def valueSet(vs: List[ValueSetValue]): NodeConstraint =
    NodeConstraint.empty.copy(values = Some(vs))

}

case class Shape(
    virtual:Option[Boolean],
    closed: Option[Boolean],
    extra: Option[List[IRI]],
    expression: Option[TripleExpr],
    inherit: Option[ShapeLabel],
    semActs: Option[List[SemAct]]
) extends ShapeExpr

object Shape{
  def empty: Shape = Shape(
   virtual = None,
   closed = None,
   extra = None,
   expression = None,
   inherit = None,
   semActs = None
  )
}

case class ShapeRef(reference: ShapeLabel) extends ShapeExpr

case class ShapeExternal() extends ShapeExpr


sealed trait XsFacet {
  val fieldName: String
}

sealed trait StringFacet extends XsFacet

case class Length(v: Int) extends StringFacet {
  val fieldName = "length"
}

case class MinLength(v:Int) extends StringFacet {
  val fieldName = "minlength"
}

case class MaxLength(v:Int) extends StringFacet {
  val fieldName = "maxlength"
}

case class Pattern(p: String) extends StringFacet {
  val fieldName = "pattern"
}

sealed trait NumericFacet extends XsFacet
case class MinInclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "mininclusive"
}
case class MinExclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "minexclusive"
}
case class MaxInclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "maxinclusive"
}
case class MaxExclusive(n: NumericLiteral) extends NumericFacet {
  val fieldName = "maxexclusive"
}
case class TotalDigits(n: Int) extends NumericFacet {
  val fieldName = "totaldigits"
}
case class FractionDigits(n: Int) extends NumericFacet {
  val fieldName = "fractiondigits"
}

sealed trait NumericLiteral
case class NumericInt(n: Int) extends NumericLiteral
case class NumericDouble(n: Double) extends NumericLiteral
case class NumericDecimal(n: BigDecimal) extends NumericLiteral

sealed trait ValueSetValue

sealed trait ObjectValue extends ValueSetValue
case class IRIValue(i: IRI) extends ObjectValue
case class StringValue(s: String) extends ObjectValue
case class DatatypeString(s: String, iri: IRI) extends ObjectValue
case class LangString(s: String, lang: String) extends ObjectValue

object ObjectValue {
  def trueValue: ObjectValue = DatatypeString("true", xsd_boolean)
  def falseValue: ObjectValue = DatatypeString("false", xsd_boolean)
  def intValue(n: Int): ObjectValue =
    DatatypeString(n.toString, xsd_integer)
  def doubleValue(d: Double): ObjectValue =
      DatatypeString(d.toString, xsd_double)
  def decimalValue(d: BigDecimal): ObjectValue =
      DatatypeString(d.toString, xsd_decimal)
}

case class Stem(stem: IRI) extends ValueSetValue
case class StemRange(stem: StemValue, exclusions: Option[List[ValueSetValue]]) extends ValueSetValue


sealed trait StemValue
case class IRIStem(iri: IRI) extends StemValue
case class Wildcard() extends StemValue


case class SemAct(name: IRI, code: Option[String])

abstract sealed trait TripleExpr

case class EachOf(
    expressions: List[TripleExpr],
    min: Option[Int],
    max: Option[Max],
    semActs: Option[List[SemAct]],
    annotations: Option[List[Annotation]]
) extends TripleExpr


case class SomeOf(
    expressions: List[TripleExpr],
    min: Option[Int],
    max: Option[Max],
    semActs: Option[List[SemAct]],
    annotations: Option[List[Annotation]]
) extends TripleExpr

case class Inclusion(include: ShapeLabel)
  extends TripleExpr

case class TripleConstraint(
    inverse: Option[Boolean],
    negated: Option[Boolean],
    predicate: IRI,
    valueExpr: Option[ShapeExpr],
    min: Option[Int],
    max: Option[Max],
    semActs: Option[List[SemAct]],
    annotations: Option[List[Annotation]]
    ) extends TripleExpr {
 lazy val isInverse = inverse.getOrElse(false)
 lazy val isNegated = negated.getOrElse(false)
 lazy val defaultMin = 1
 lazy val defaultMax = IntMax(1)

 lazy val minValue = min.getOrElse(defaultMin)
 lazy val maxValue = max.getOrElse(defaultMax)
}

object TripleConstraint {
  def emptyPred(pred: IRI): TripleConstraint =
    TripleConstraint(
      None,None,pred,None,None,None,None,None
    )
}

case class Annotation(predicate: IRI, obj: ObjectValue)

abstract sealed trait Max {
  def show = this match {
      case IntMax(v) => v.toString
      case Star => "*"
    }
}
case object Star extends Max
case class IntMax(v: Int) extends Max

sealed trait NodeKind
case object IRIKind extends NodeKind
case object BNodeKind extends NodeKind
case object NonLiteralKind extends NodeKind
case object LiteralKind extends NodeKind

abstract sealed trait ShapeLabel
case class IRILabel(iri: IRI) extends ShapeLabel
case class BNodeLabel(bnode: BNodeId) extends ShapeLabel

object Schema {
  def empty: Schema =
    Schema(None,None,None,None,None)

    /**
      * If prefixMap contains a: -> http://example.org/
      * then qualify(IRI("http://example.org/x")) = "a:x"
      */
  def qualify(iri: IRI, pm: Map[Prefix,IRI]): String = {
      def startsWithPredicate(p: (Prefix, IRI)): Boolean = {
        iri.str.startsWith(p._2.str)
      }

      pm.find(startsWithPredicate) match {
        case None => "<" ++ iri.str ++ ">"
        case Some(p) => p._1.s + iri.str.stripPrefix(p._2.str)
      }
  }

}
