package es.weso.shex.implicits
import io.circe._
import io.circe.syntax._
import es.weso.rdf.nodes._
import cats.implicits._
import es.weso.shex._
import showShEx._
import es.weso.rdf._

object encoderShEx {

  implicit lazy val encodeSchema: Encoder[Schema] = new Encoder[Schema] {
    final def apply(s: Schema): Json =
      mkObjectTyped(
        "Schema",
        List(
          field("@context", "https://shexspec.github.io/context.jsonld"),
          optFieldMap("prefixes", s.prefixes.map(_.pm)),
          optField("base", s.base),
          optField("startActs", s.startActs),
          optField("start", s.start),
          optField("shapes", s.shapes)))
  }

  implicit lazy val encoderPrefix = new Encoder[Prefix] {
    final def apply(p: Prefix): Json = Json.fromString(p.str)
  }

  implicit lazy val keyEncoderPrefix: KeyEncoder[Prefix] = new KeyEncoder[Prefix] {
    final def apply(p: Prefix): String = p.str
  }

  implicit lazy val keyEncoderShapeLabel: KeyEncoder[ShapeLabel] = new KeyEncoder[ShapeLabel] {
    final def apply(p: ShapeLabel): String = p.show
  }

  implicit lazy val encoderIRI: Encoder[IRI] = new Encoder[IRI] {
    final def apply(iri: IRI): Json = Json.fromString(iri.str)
  }

  implicit lazy val encoderSemAct: Encoder[SemAct] = new Encoder[SemAct] {
    final def apply(a: SemAct): Json = {
      mkObjectTyped(
        "SemAct",
        List(
          field("name", a.name),
          optField("code", a.code)))
    }
  }

  implicit lazy val encoderMax: Encoder[Max] = new Encoder[Max] {
    final def apply(a: Max): Json = a match {
      case Star => Json.fromString("*")
      case IntMax(n) => Json.fromInt(n)
    }
  }

  implicit lazy val encoderAnnotation: Encoder[Annotation] = new Encoder[Annotation] {
    final def apply(a: Annotation): Json =
      mkObjectTyped(
        "Annotation",
        List(
          field("predicate", a.predicate),
          field("object", a.obj)))
  }

  implicit lazy val encoderTripleConstraint: Encoder[TripleConstraint] = new Encoder[TripleConstraint] {
    final def apply(a: TripleConstraint): Json =
      mkObjectTyped(
        "TripleConstraint",
        List(
          optField("id", a.id),
          optField("inverse", a.optInverse),
          optField("negated", a.optNegated),
          field("predicate", a.predicate),
          optFieldIfNotDefault("valueExpr", a.valueExpr, ShapeExpr.any),
          optField("min", a.optMin),
          optField("max", a.optMax),
          optField("semActs", a.semActs),
          optField("annotations", a.annotations)))
  }

  implicit lazy val encodeShapeExpr: Encoder[ShapeExpr] = new Encoder[ShapeExpr] {
    final def apply(a: ShapeExpr): Json =
      a match {
        case ShapeOr(id, ses) =>
          mkObjectTyped("ShapeOr", List(optField("id", id), field("shapeExprs", ses)))

        case ShapeAnd(id, ses) =>
          mkObjectTyped("ShapeAnd", List(optField("id", id), field("shapeExprs", ses)))

        case ShapeNot(id, se) =>
          mkObjectTyped("ShapeNot", List(optField("id", id), field("shapeExpr", se)))

        case nc: NodeConstraint => nc.asJson
        case s: Shape => s.asJson
        case ShapeRef(r) => r.asJson
        case ShapeExternal(id) => mkObjectTyped("ShapeExternal", List(optField("id", id)))
      }
  }

  implicit lazy val encodeNodeConstraint: Encoder[NodeConstraint] = new Encoder[NodeConstraint] {
    final def apply(a: NodeConstraint): Json =
      mkObjectTyped(
        "NodeConstraint",
        List(
          optField("id", a.id),
          optField("nodeKind", a.nodeKind),
          optField("datatype", a.datatype),
          optField("values", a.values)) ++ mkFieldsFacets(a.xsFacets))
  }

  implicit lazy val encodeShape: Encoder[Shape] = new Encoder[Shape] {
    final def apply(a: Shape): Json =
      mkObjectTyped(
        "Shape",
        List(
          optField("id", a.id),
          optFieldIfNotDefault("virtual", a.virtual, Shape.defaultVirtual),
          optFieldIfNotDefault("closed", a.closed, Shape.defaultClosed),
          optField("extra", a.extra),
          optField("expression", a.expression),
          optField("inherit", a.inherit),
          optField("semActs", a.semActs)))
  }

  implicit lazy val encodeShapeLabel: Encoder[ShapeLabel] = new Encoder[ShapeLabel] {
    final def apply(a: ShapeLabel): Json = a match {
      case IRILabel(iri) => iri.asJson
      case BNodeLabel(bNode) => Json.fromString("_:" + bNode.id)
    }
  }
  implicit lazy val encodeTripleExpr: Encoder[TripleExpr] = new Encoder[TripleExpr] {
    final def apply(a: TripleExpr): Json = a match {
      case s: OneOf => s.asJson
      case s: EachOf => s.asJson
      case Inclusion(i) => i.asJson // mkObjectTyped("Inclusion",List(field("include",i)))
      case tc: TripleConstraint => tc.asJson
    }
  }

  implicit lazy val encodeEachOf: Encoder[EachOf] = new Encoder[EachOf] {
    final def apply(a: EachOf): Json =
      mkObjectTyped(
        "EachOf",
        List(
          field("expressions", a.expressions),
          optField("id", a.id),
          optField("min", a.optMin),
          optField("max", a.optMax),
          optField("semActs", a.semActs),
          optField("annotations", a.annotations)))
  }

  implicit lazy val encodeOneOf: Encoder[OneOf] = new Encoder[OneOf] {
    final def apply(a: OneOf): Json =
      mkObjectTyped(
        "OneOf",
        List(
          field("expressions", a.expressions),
          optField("id", a.id),
          optField("min", a.optMin),
          optField("max", a.optMax),
          optField("semActs", a.semActs),
          optField("annotations", a.annotations)))
  }

  def mkFieldsFacets(xs: List[XsFacet]): List[Option[(String, Json)]] = {
    xs.map(x => Some(mkFieldFacet(x)))
  }

  def mkFieldFacet(x: XsFacet): (String, Json) =
    x match {
      case Length(v) => (x.fieldName, Json.fromInt(v))
      case MinLength(v) => (x.fieldName, Json.fromInt(v))
      case MaxLength(v) => (x.fieldName, Json.fromInt(v))
      case Pattern(p, flags) => if (!flags.isDefined) {
        (x.fieldName, Json.fromString(p))
      } else {
       throw new Exception(s"Unimplemented encoder of pattern with flags yet: $p, flags: $flags")
      }
      case MinInclusive(n) => (x.fieldName, encodeNumeric(n))
      case MaxInclusive(n) => (x.fieldName, encodeNumeric(n))
      case MinExclusive(n) => (x.fieldName, encodeNumeric(n))
      case MaxExclusive(n) => (x.fieldName, encodeNumeric(n))
      case TotalDigits(n) => (x.fieldName, Json.fromInt(n))
      case FractionDigits(n) => (x.fieldName, Json.fromInt(n))
    }

  implicit lazy val encodeNumeric: Encoder[NumericLiteral] = new Encoder[NumericLiteral] {
    final def apply(a: NumericLiteral): Json = a match {
      case NumericInt(n) => Json.fromInt(n)
      case NumericDouble(d) => Json.fromDoubleOrString(d)
      case NumericDecimal(d) => Json.fromBigDecimal(d)
    }
  }

  implicit lazy val encodeNodeKind: Encoder[NodeKind] = new Encoder[NodeKind] {
    final def apply(a: NodeKind): Json = Json.fromString(a match {
      case IRIKind => "iri"
      case BNodeKind => "bnode"
      case NonLiteralKind => "nonliteral"
      case LiteralKind => "literal"
    })
  }

  implicit lazy val encodeValueSetValue: Encoder[ValueSetValue] = new Encoder[ValueSetValue] {
    final def apply(a: ValueSetValue): Json = a match {
      case ov: ObjectValue => ov.asJson
      case IRIStem(s) => mkObjectTyped("Stem", List(field("stem", s)))
      case IRIStemRange(s, exclusions) =>
        mkObjectTyped("StemRange", List(field("stem", s), optField("exclusions", exclusions)))
    }
  }

  implicit lazy val encodeObjectValue: Encoder[ObjectValue] = new Encoder[ObjectValue] {
    final def apply(a: ObjectValue): Json = a match {
      case IRIValue(i) => i.asJson
      case StringValue(s) => {
        val fields: List[(String, Json)] = List(("value", Json.fromString(s)))
        Json.fromFields(fields)
      }
      case DatatypeString(s, d) => {
        val fields: List[(String, Json)] = List(
          ("value", Json.fromString(s)),
          ("type", d.asJson))
        Json.fromFields(fields)
      }
      case LangString(s, l) => {
        val fields: List[(String, Json)] = List(
          ("value", Json.fromString(s)),
          ("language", Json.fromString(l)))
        Json.fromFields(fields)
      }
    }
  }

  implicit lazy val encodeStemValue: Encoder[IRIStemRangeValue] = new Encoder[IRIStemRangeValue] {
    final def apply(a: IRIStemRangeValue): Json = a match {
      case IRIStemValueIRI(i) => i.asJson
      case IRIStemWildcard() => mkObjectTyped("Wildcard", List())
    }
  }

  // Utils...

  def encodeOptFieldAsMap[A](name: String, m: Option[A])(implicit encoder: Encoder[A]): Map[String, Json] =
    m match {
      case None => Map()
      case Some(v) => Map(name -> encoder(v))
    }

  def field[A: Encoder](name: String, v: A): Option[(String, Json)] = {
    val encoder = implicitly[Encoder[A]]
    Some((name, encoder(v)))
  }

  def optField[A: Encoder](name: String, m: Option[A]): Option[(String, Json)] = {
    m match {
      case None => None
      case Some(v) => field(name, v)
    }
  }

  def optFieldIfNotDefault[A: Encoder](
    name: String,
    m: Option[A],
    default: A): Option[(String, Json)] = {
    m match {
      case None => None
      case Some(v) =>
        if (v == default) None
        else field(name, v)
    }
  }

  def optFieldMap[A: KeyEncoder, B: Encoder](name: String, m: Option[Map[A, B]]): Option[(String, Json)] = {
    m match {
      case None => None
      case Some(mapValue) => {
        val encoder = implicitly[Encoder[Map[A, B]]]
        Some((name, encoder(mapValue)))
      }
    }
  }

  def mkObjectTyped(typeName: String, fields: List[Option[(String, Json)]]): Json = {
    val map = Map("type" -> Json.fromString(typeName)) ++
      fields.filter(_.isDefined).sequence.getOrElse(List()).toMap
    Json.fromJsonObject(JsonObject.fromMap(map))
  }

}
