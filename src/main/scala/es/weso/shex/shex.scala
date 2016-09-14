package es.weso.shex
import es.weso.rdf.nodes._

case class Schema(
    prefixes: Option[Map[Prefix,IRI]],
    base: Option[IRI],
    startActs: Option[List[SemAct]],
    start: Option[ShapeExpr],
    shapes: Option[Map[ShapeLabel,ShapeExpr]]
)

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

case class Shape(
    virtual:Option[Boolean],
    closed: Option[Boolean],
    extra: Option[List[IRI]],
    expression: Option[TripleExpr],
    inherit: Option[ShapeLabel],
    semActs: Option[List[SemAct]]
) extends ShapeExpr

case class ShapeRef(reference: ShapeLabel) extends ShapeExpr

case class ShapeExternal() extends ShapeExpr

object NodeConstraint {

  def empty = NodeConstraint(
      nodeKind = None,
      datatype = None,
      xsFacets = List(),
      values = None
  )

  def nodeKind(nk: NodeKind): NodeConstraint =
    NodeConstraint.empty.copy(nodeKind = Some(nk))

}

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
    semActs: Option[List[SemAct]]
    ) extends TripleExpr

case class Annotation(predicate: IRI, obj: ObjectValue)

abstract sealed trait Max
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


object ShEx {

}
