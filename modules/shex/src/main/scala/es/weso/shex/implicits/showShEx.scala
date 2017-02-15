package es.weso.shex.implicits
import cats._
import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.rdf._

object showShEx {

implicit lazy val showSchema: Show[Schema] = new Show[Schema] {
  final def show(s: Schema): String = {
    s"Schema(${optShow(s.prefixes)}, ${optShow(s.base)}, ${optShow(s.startActs)}, ${optShow(s.start)}, ${optShow(s.shapes)})"
  }
}

implicit lazy val showPrefixMap: Show[PrefixMap] = new Show[PrefixMap] {
  final def show(pm: PrefixMap): String = {
    s"PrefixMap($pm)"
  }
}


implicit lazy val showShapeExpr: Show[ShapeExpr] = new Show[ShapeExpr] {
  final def show(a: ShapeExpr): String = a match {
    case ShapeOr(shapes) => s"ShapeOr(${shapes.map(_.show).mkString(",")})"
    case ShapeAnd(shapes) => s"ShapeAnd(${shapes.map(_.show).mkString(",")})"
    case ShapeNot(shape) => s"ShapeNot(${shape.show})"
    case s: Shape => s.show
    case nc:NodeConstraint => nc.show
    case ShapeRef(r) => s"ShapeRef(${r.show})"
    case ShapeExternal() => s"ShapeExternal"
  }
}

implicit lazy val showShape: Show[Shape] = new Show[Shape] {
  final def show(a: Shape): String =
    s"Shape(${optShow(a.virtual)}, ${optShow(a.closed)}, ${optShow(a.extra)}, ${optShow(a.expression)}, ${optShow(a.inherit)}, ${optShow(a.semActs)})"
}

implicit lazy val showNodeConstraint: Show[NodeConstraint] = new Show[NodeConstraint] {
  final def show(a: NodeConstraint): String =
    s"NodeConstraint(${optShow(a.nodeKind)}, ${optShow(a.datatype)}, ${a.xsFacets.show}, ${optShow(a.values)})"
}

implicit lazy val showNodeKind: Show[NodeKind] = new Show[NodeKind] {
  final def show(a: NodeKind): String = a match {
    case IRIKind => "iri"
    case BNodeKind => "bnode"
    case NonLiteralKind => "nonLiteral"
    case LiteralKind => "literal"
  }
}

implicit lazy val showValueSetValue: Show[ValueSetValue] = new Show[ValueSetValue] {
  final def show(a: ValueSetValue): String = a match {
    case IRIValue(iri) => iri.show
    case StringValue(s) => "\"" + s + "\""
    case DatatypeString(s,d) => "\"" + s + "\"^^" + d.show
    case LangString(s,l) => "\"" + s + "\"@" + l
    case Stem(s) => s"stem($s)"
    case StemRange(s,exclusions) => s"stemRange(${s.show},${optShow(exclusions)})"
  }
}

implicit lazy val showStemValue: Show[StemValue] = new Show[StemValue] {
  final def show(a: StemValue): String = a match {
    case IRIStem(i) => i.show
    case Wildcard() => "*"
  }
}

implicit lazy val showMax = new Show[Max] {
  final def show(a: Max): String = a match {
   case Star => "*"
   case IntMax(n) => n.show
  }
}

implicit lazy val showIRI : Show[IRI] = new Show[IRI] {
  final def show(iri: IRI): String = 
    iri.str
}

implicit lazy val showPrefix : Show[Prefix] = new Show[Prefix] {
  final def show(p: Prefix): String = p.str
}


implicit lazy val showSemAct : Show[SemAct] = new Show[SemAct] {
  final def show(a: SemAct): String =
    "SemAct(" + a.name.show + "," + optShow(a.code) + ")"
}

implicit lazy val showXsFacet: Show[XsFacet] = new Show[XsFacet] {
  final def show(a: XsFacet): String = a match {
    case Length(v) => s"${a.fieldName}(${v.show})"
    case MinLength(v) => s"${a.fieldName}(${v.show})"
    case MaxLength(v) => s"${a.fieldName}(${v.show})"
    case Pattern(v) => s"${a.fieldName}(${v.show})"
    case MinInclusive(n) => s"${a.fieldName}(${n.show})"
    case MaxInclusive(n) => s"${a.fieldName}(${n.show})"
    case MinExclusive(n) => s"${a.fieldName}(${n.show})"
    case MaxExclusive(n) => s"${a.fieldName}(${n.show})"
    case TotalDigits(n) => s"${a.fieldName}(${n.show})"
    case FractionDigits(n) => s"${a.fieldName}(${n.show})"
  }
}

implicit lazy val showNumericLiteral: Show[NumericLiteral] = new Show[NumericLiteral] {
  final def show(a: NumericLiteral): String = a match {
    case NumericInt(n) => n.show
    case NumericDouble(n) => n.show
    case NumericDecimal(n) => n.show
  }
}

implicit lazy val showTripleExpr: Show[TripleExpr] = new Show[TripleExpr] {
  final def show(a: TripleExpr): String = a match {
    case e: EachOf => e.show
    case e: OneOf => e.show
    case Inclusion(i) => s"Inclusion(${i.show})"
    case tc: TripleConstraint => tc.show
  }
}

implicit lazy val showEachOf: Show[EachOf] = new Show[EachOf] {
  final def show(a: EachOf): String =
    s"EachOf(${a.expressions.show}, ${optShow(a.optMin)}, ${optShow(a.optMax)}, ${optShow(a.semActs)}, ${optShow(a.annotations)})"
}

implicit lazy val showSomeOf: Show[OneOf] = new Show[OneOf] {
  final def show(a: OneOf): String =
    s"OneOf(${a.expressions.show}, ${optShow(a.optMin)}, ${optShow(a.optMax)}, ${optShow(a.semActs)}, ${optShow(a.annotations)})"
}

implicit lazy val showTripleConstraint: Show[TripleConstraint] = new Show[TripleConstraint] {
  final def show(a: TripleConstraint): String =
    s"TripleConstraint(${optShow(a.optInverse)}, ${optShow(a.optNegated)}, ${a.predicate.show}, ${a.valueExpr.show}, ${optShow(a.optMin)}, ${optShow(a.optMax)}, ${optShow(a.semActs)}, ${optShow(a.annotations)})"
}

implicit lazy val showAnnotation: Show[Annotation] = new Show[Annotation] {
  final def show(a: Annotation): String =
    s"Annotation(${a.predicate.show}, ${a.obj.show})"
}

implicit lazy val showObjectValue: Show[ObjectValue] = new Show[ObjectValue] {
  final def show(a: ObjectValue): String = a match {
    case IRIValue(i) => i.show
    case StringValue(s) => "\"" + s + "\""
    case DatatypeString(s,iri) => "\"" + s + "\"^^" + iri.show
    case LangString(s,l) => "\"" + s + "\"@" + l
  }
}

implicit lazy val showShapeLabel: Show[ShapeLabel] = new Show[ShapeLabel] {
  final def show(a: ShapeLabel): String = a match {
    case IRILabel(iri) => iri.show
    case BNodeLabel(bnode) => "_:" + bnode.id
  }

}

def optShow[A: Show](m: Option[A]): String =
  m match {
  case None => "-"
  case Some(v) => v.show
}


}
