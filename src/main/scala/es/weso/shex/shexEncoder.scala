package es.weso.shex
import io.circe._
import io.circe.syntax._
import cats.data._
import es.weso.rdf.nodes._
import cats._
import cats.implicits._

object shexEncoder {

  implicit lazy val encodeSchema= new Encoder[Schema] {
   final def apply(s: Schema): Json = ??? 
  }

  implicit lazy val encodePrefix = new Encoder[Prefix] {
   final def apply(p: Prefix): Json = ??? 
  }
  
  implicit lazy val encodeIRI = new Encoder[IRI] {
   final def apply(iri: IRI): Json = Json.fromString(iri.str) 
  }


  implicit lazy val encodeSemAct = new Encoder[SemAct] {
   final def apply(a: SemAct): Json = {
    mkObjectTyped("SemAct",
        List(field("name", a.name),
             optField("code", a.code))
   )
  }
  }
 
 implicit lazy val encodeMax: Encoder[Max] = new Encoder[Max] {
  final def apply(a: Max): Json = a match {
   case Star => Json.fromString("*")
   case IntMax(n) => Json.fromInt(n)
  }
 }

 implicit lazy val encodeAnnotation: Encoder[Annotation] = new Encoder[Annotation] {
  final def apply(a: Annotation): Json = 
    mkObjectTyped("Annotation",
        List(field("predicate",a.predicate),
             field("object",a.obj)))
 }
 
 implicit lazy val encodeTripleConstraint: Encoder[TripleConstraint] = new Encoder[TripleConstraint] {
  final def apply(a: TripleConstraint): Json = 
        mkObjectTyped("TripleConstraint",
        List(optField("inverse", a.inverse),
             optField("negated", a.negated),
             field("predicate", a.predicate),
             field("valueExpr", a.valueExpr),
             optField("min", a.min),
             optField("max", a.max),
             optField("semActs", a.semActs)
             )
   )
 }

implicit lazy val encodeShapeExpr: Encoder[ShapeExpr] = new Encoder[ShapeExpr] {
  final def apply(a: ShapeExpr): Json =
    a match {
    case ShapeOr(ses) => 
      mkObjectTyped("ShapeOr", List(field("shapeExprs",ses)))
    
    case ShapeAnd(ses) => 
      mkObjectTyped("ShapeAnd", List(field("shapeExprs",ses)))

    case ShapeNot(se) => 
      mkObjectTyped("ShapeNot", List(field("shapeExpr",se)))

    case nc : NodeConstraint => nc.asJson
    case s: Shape => s.asJson
    case ShapeRef(r) => mkObjectTyped("ShapeRef",List(field("reference",r)))
    case ShapeExternal() => mkObjectTyped("ShapeExternal",List())
  }
}

implicit lazy val encodeNodeConstraint: Encoder[NodeConstraint] = new Encoder[NodeConstraint] {
  final def apply(a: NodeConstraint): Json =  
    mkObjectTyped("NodeConstraint", 
        List(optField("nodeKind",a.nodeKind),
             optField("datatype",a.datatype),
             optField("values",a.values)
            ) ++ mkFieldsFacets(a.xsFacets)) 
}

implicit lazy val encodeShape: Encoder[Shape] = new Encoder[Shape] {
  final def apply(a: Shape): Json =  
    mkObjectTyped("Shape", 
        List(optField("virtual",a.virtual),
             optField("closed",a.closed),
             optField("extra",a.extra),
             optField("expression",a.expression),
             optField("inherit",a.inherit),
             optField("semActs",a.semActs)
            )) 
}

implicit lazy val encodeShapeLabel: Encoder[ShapeLabel] = new Encoder[ShapeLabel] {
  final def apply(a: ShapeLabel): Json =  a match {
    case IRILabel(iri) => iri.asJson
    case BNodeLabel(bNode) => Json.fromString("_:" + bNode.id)
  }
}
implicit lazy val encodeTripleExpr: Encoder[TripleExpr] = new Encoder[TripleExpr] {
  final def apply(a: TripleExpr): Json =  a match {
    case s: SomeOf => s.asJson
    case s: EachOf => s.asJson
    case Inclusion(i) => mkObjectTyped("Inclusion",List(field("include",i)))
    case tc: TripleConstraint => tc.asJson 
  }
}

implicit lazy val encodeEachOf: Encoder[EachOf] = new Encoder[EachOf] {
  final def apply(a: EachOf): Json =
    mkObjectTyped("EachOf",
        List(field("expressions",a.expressions),
             optField("min",a.min),
             optField("max",a.max),
             optField("semActs",a.semActs),
             optField("annotations",a.annotations)
    ))
}

implicit lazy val encodeSomeOf: Encoder[SomeOf] = new Encoder[SomeOf] {
  final def apply(a: SomeOf): Json =
    mkObjectTyped("SomeOf",
        List(field("expressions",a.expressions),
             optField("min",a.min),
             optField("max",a.max),
             optField("semActs",a.semActs),
             optField("annotations",a.annotations)
    ))
}

def mkFieldsFacets(xs: List[XsFacet]): List[Option[(String,Json)]] = {
  xs.map(x => Some(mkFieldFacet(x)))
}

def mkFieldFacet(x: XsFacet): (String,Json) = 
  x match {
  case Length(v) => (x.fieldName,Json.fromInt(v))
  case MinLength(v) => (x.fieldName,Json.fromInt(v))
  case MaxLength(v) => (x.fieldName,Json.fromInt(v))
  case Pattern(p) => (x.fieldName,Json.fromString(p))
  case MinInclusive(n) => (x.fieldName,encodeNumeric(n))
  case MaxInclusive(n) => (x.fieldName,encodeNumeric(n))
  case MinExclusive(n) => (x.fieldName,encodeNumeric(n))
  case MaxExclusive(n) => (x.fieldName,encodeNumeric(n))
  case TotalDigits(n) => (x.fieldName,Json.fromInt(n))
  case FractionDigits(n) => (x.fieldName,Json.fromInt(n))
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
    case Stem(s) => mkObjectTyped("Stem",List(field("stem",s)))
    case StemRange(s,exclusions) => 
      mkObjectTyped("StemRange",List(field("stem",s), optField("exclusiones", exclusions)))
  }
}

implicit lazy val encodeObjectValue: Encoder[ObjectValue] = new Encoder[ObjectValue] {
  final def apply(a: ObjectValue): Json = a match {
    case IRIValue(i) => i.asJson
    case StringValue(s) => Json.fromString("\"" + s + "\"")
    case DatatypeString(s,d) => Json.fromString("\"" + s + "\"^^" + d.str)
    case LangString(s,l) => Json.fromString("\"" + s + "\"@" + l)
  }
}


implicit lazy val encodeStemValue: Encoder[StemValue] = new Encoder[StemValue] {
  final def apply(a: StemValue): Json = a match {
    case IRIStem(i) => i.asJson
    case Wildcard() => mkObjectTyped("Wildcard",List())
  }
}

// Utils...
  
def encodeOptFieldAsMap[A](name: String, m: Option[A])(implicit encoder: Encoder[A]): Map[String,Json] = 
 m match {
      case None => Map()
      case Some(v) => Map(name -> encoder(v))
 }

def field[A: Encoder](name: String, v: A): Option[(String, Json)] = {
  val encoder = implicitly[Encoder[A]] 
  Some(name, encoder(v))
}

def optField[A: Encoder](name: String, m: Option[A]): Option[(String,Json)] = {
  m match {
    case None => None
    case Some(v) => field(name,v)
  }
}

def mkObjectTyped(typeName: String, fields: List[Option[(String,Json)]]): Json = {
  val map = Map("type" -> Json.fromString(typeName)) ++
            fields.filter(_.isDefined).sequence.getOrElse(List()).toMap
  Json.fromJsonObject(JsonObject.fromMap(map))
}

}