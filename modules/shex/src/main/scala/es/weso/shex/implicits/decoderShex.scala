package es.weso.shex.implicits
import io.circe._
import io.circe.syntax._
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.nodes._
import util.matching._
import java.net.URISyntaxException
import es.weso.shex._
import es.weso.rdf._

object decoderShEx {

  implicit lazy val decodeSchema: Decoder[Schema] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "Schema")
      prefixes <- optFieldDecodeMap[Prefix, IRI](c, "prefixes")
      base <- optFieldDecode[IRI](c, "base")
      startActs <- optFieldDecode[List[SemAct]](c, "startActs")
      start <- optFieldDecode[ShapeExpr](c, "start")
      shapes <- optFieldDecodeMap[ShapeLabel, ShapeExpr](c, "shapes")
    } yield Schema(prefixes.map(PrefixMap(_)), base, startActs, start, shapes)
  }

  implicit lazy val decodePrefix: Decoder[Prefix] =
    Decoder[String].map(Prefix(_))

  implicit lazy val keyDecoderPrefix: KeyDecoder[Prefix] =
    KeyDecoder.instance { str => parsePrefix(str).toOption }

  implicit lazy val decodeShapeLabel: Decoder[ShapeLabel] =
    Decoder[String].emap(str => parseShapeLabel(str))

  implicit lazy val keyDecoderShapeLabel: KeyDecoder[ShapeLabel] =
    KeyDecoder.instance { str => parseShapeLabel(str).toOption }

  implicit lazy val decodeBNodeId: Decoder[BNodeId] =
    Decoder[String].map(BNodeId(_))

  implicit lazy val decodeIRI: Decoder[IRI] =
    Decoder[String].emap(parseIRI(_))

  implicit lazy val decodeSemAct: Decoder[SemAct] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "SemAct")
      name <- fieldDecode[IRI](c, "name")
      code <- optFieldDecode[String](c, "code")
    } yield SemAct(name, code)
  }

  implicit lazy val decodeMax: Decoder[Max] =
    Decoder[Int].map(n => IntMax(n)).or(
      Decoder[String].map(s => Star))

  implicit lazy val decodeShapeExpr: Decoder[ShapeExpr] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "ShapeOr"        => c.as[ShapeOr]
      case "ShapeAnd"       => c.as[ShapeAnd]
      case "ShapeNot"       => c.as[ShapeNot]
      case "NodeConstraint" => c.as[NodeConstraint]
      case "Shape"          => c.as[Shape]
      case "ShapeRef"       => c.as[ShapeRef]
      case "ShapeExternal"  => c.as[ShapeExternal]
      case other =>
        Xor.left(DecodingFailure(s"Decoding ShapeExpr. Unexpected value $other", Nil))
    }
  }

  implicit lazy val decodeShapeOr: Decoder[ShapeOr] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "ShapeOr")
      ses <- fieldDecode[List[ShapeExpr]](c, "shapeExprs")
    } yield ShapeOr(ses)
  }

  implicit lazy val decodeShapeAnd: Decoder[ShapeAnd] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "ShapeAnd")
      ses <- fieldDecode[List[ShapeExpr]](c, "shapeExprs")
    } yield ShapeAnd(ses)
  }

  implicit lazy val decodeShapeNot: Decoder[ShapeNot] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "ShapeNot")
      se <- fieldDecode[ShapeExpr](c, "shapeExpr")
    } yield ShapeNot(se)
  }

  implicit lazy val decodeNodeConstraint: Decoder[NodeConstraint] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "NodeConstraint")
      nodeKind <- optFieldDecode[NodeKind](c, "nodeKind")
      datatype <- optFieldDecode[IRI](c, "datatype")
      values <- optFieldDecode[List[ValueSetValue]](c, "values")
      xsFacets <- getXsFacets(c)
    } yield NodeConstraint(nodeKind, datatype, xsFacets, values)
  }

  def getXsFacets(c: HCursor): Decoder.Result[List[XsFacet]] = {
    val fields: List[String] = c.fields.getOrElse(List())
    val rs : List[Xor[DecodingFailure, Option[XsFacet]]] =
       fields.map(extractXsFacet(_, c))
    sequenceXor(rs)
//    val r: Xor[DecodingFailure, List[Option[XsFacet]]] = rs.sequence
    /*rs match {
      case Xor.Left(e)   => Xor.Left(e)
      case Xor.Right(ls) => Xor.Right(ls.filter(_.isDefined).sequence.getOrElse(List()))
    }*/

  }

  // Todo: Use "sequence" when I find why it gives a type error...
  def sequenceXor[E,A](xs: List[Xor[E,Option[A]]]): Xor[E,List[A]] = {
    val zero: Xor[E,List[A]] = Xor.right(List())
    def next(r: Xor[E,List[A]], x: Xor[E,Option[A]]): Xor[E,List[A]] =
      x match {
        case Xor.Left(e) => Xor.Left(e)
        case Xor.Right(None) => r
        case Xor.Right(Some(v)) => r match {
          case Xor.Left(e) => Xor.Left(e)
          case Xor.Right(vs) => Xor.Right(v :: vs)
        }
      }
    xs.foldLeft(zero)(next)
  }

  def extractXsFacet(name: String, c: HCursor): Xor[DecodingFailure, Option[XsFacet]] = {
    name match {
      case "length"       => c.get[Int](name).map(n => Some(Length(n)))
      case "minlength"    => c.get[Int](name).map(n => Some(MinLength(n)))
      case "maxlength"    => c.get[Int](name).map(n => Some(MaxLength(n)))
      case "pattern"      => c.get[String](name).map(p => Some(Pattern(p)))
      case "mininclusive" => c.get[NumericLiteral](name).map(p => Some(MinInclusive(p)))
      case "minexclusive" => c.get[NumericLiteral](name).map(p => Some(MinExclusive(p)))
      case "maxinclusive" => c.get[NumericLiteral](name).map(p => Some(MaxInclusive(p)))
      case "maxexclusive" => c.get[NumericLiteral](name).map(p => Some(MaxExclusive(p)))
      case "fractiondigits" => c.get[Int](name).map(p => Some(FractionDigits(p)))
      case "totaldigits" => c.get[Int](name).map(p => Some(TotalDigits(p)))
      case _              => None.right
    }
  }

  implicit lazy val decodeNumericLiteral: Decoder[NumericLiteral] =
    Decoder[Int].map(n => NumericInt(n)).or(
      Decoder[Double].map(n => NumericDouble(n)).or(
        Decoder[BigDecimal].map(n => NumericDecimal(n))))

  implicit lazy val decodeNodeKind: Decoder[NodeKind] = Decoder.instance { c =>
    c.field("nodeKind").as[String].flatMap {
      case "iri"        => IRIKind.right
      case "bnode"      => BNodeKind.right
      case "nonliteral" => NonLiteralKind.right
      case "literal"    => LiteralKind.right
      case other =>
        Xor.left(DecodingFailure(s"Decoding nodeKind. Unexpected value $other", Nil))
    }
  }

  implicit lazy val decodeShape: Decoder[Shape] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "Shape")
      virtual <- optFieldDecode[Boolean](c, "virtual")
      closed <- optFieldDecode[Boolean](c, "closed")
      extra <- optFieldDecode[List[IRI]](c, "extra")
      expression <- optFieldDecode[TripleExpr](c, "expression")
      inherit <- optFieldDecode[ShapeLabel](c, "inherit")
      semActs <- optFieldDecode[List[SemAct]](c, "semActs")
    } yield Shape(virtual, closed, extra, expression, inherit, semActs)
  }

  implicit lazy val decodeTripleExpr: Decoder[TripleExpr] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "EachOf"           => c.as[EachOf]
      case "SomeOf"           => c.as[SomeOf]
      case "Inclusion"        => c.as[Inclusion]
      case "TripleConstraint" => c.as[TripleConstraint]
      case other =>
        Xor.left(DecodingFailure(s"Decoding TripleExpr. Unexpected value $other", Nil))
    }
  }

  implicit lazy val decodeEachOf: Decoder[EachOf] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "EachOf")
      expressions <- fieldDecode[List[TripleExpr]](c, "expressions")
      min <- optFieldDecode[Int](c, "min")
      max <- optFieldDecode[Max](c, "max")
      semActs <- optFieldDecode[List[SemAct]](c, "semActs")
      annotations <- optFieldDecode[List[Annotation]](c, "annotations")
    } yield EachOf(expressions, min, max, semActs, annotations)
  }

  implicit lazy val decodeSomeOf: Decoder[SomeOf] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "SomeOf")
      expressions <- fieldDecode[List[TripleExpr]](c, "expressions")
      min <- optFieldDecode[Int](c, "min")
      max <- optFieldDecode[Max](c, "max")
      semActs <- optFieldDecode[List[SemAct]](c, "semActs")
      annotations <- optFieldDecode[List[Annotation]](c, "annotations")
    } yield SomeOf(expressions, min, max, semActs, annotations)
  }

  implicit lazy val decodeAnnotation: Decoder[Annotation] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "Annotation")
      predicate <- fieldDecode[IRI](c, "predicate")
      obj <- fieldDecode[ObjectValue](c, "object")
    } yield Annotation(predicate, obj)
  }

  implicit lazy val decodeInclusion: Decoder[Inclusion] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "Inclusion")
      include <- fieldDecode[ShapeLabel](c, "include")
    } yield Inclusion(include)
  }

  implicit lazy val decodeShapeRef: Decoder[ShapeRef] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "ShapeRef")
      reference <- fieldDecode[ShapeLabel](c, "reference")
    } yield ShapeRef(reference)
  }

  implicit lazy val decodeShapeExternal: Decoder[ShapeExternal] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "ShapeExternal")
    } yield ShapeExternal()
  }

  implicit lazy val decodeValueSetValue: Decoder[ValueSetValue] =
    Decoder[Stem].map(identity).or(
      Decoder[StemRange].map(identity).or(
        Decoder[ObjectValue].map(identity)))

  implicit lazy val decodeObjectValue: Decoder[ObjectValue] =
    Decoder[String].emap(s => parseObjectValue(s))

  def parseObjectValue(s: String): Xor[String, ObjectValue] = {
    s match {
      case langRegex(s, l)     => LangString(s, l).right
      case datatypeRegex(s, d) => parseIRI(d).map(i => DatatypeString(s, i))
      case stringRegex(str)    => StringValue(str).right
      case iriRegex(str)         => parseIRI(str).map(i => IRIValue(i))
      case _                   => Xor.Left(s"$s doesn't match valueset value")
    }
  }

  implicit lazy val decodeIRIValue: Decoder[IRIValue] =
    Decoder[String].emap(s => parseIRI(s)).map(i => IRIValue(i))

  implicit lazy val decodeStem: Decoder[Stem] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "Stem")
      stem <- fieldDecode[IRI](c, "stem")
    } yield Stem(stem)
  }

  implicit lazy val decodeStemRange: Decoder[StemRange] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "StemRange")
      stem <- fieldDecode[StemValue](c, "stem")
      exclusions <- optFieldDecode[List[ValueSetValue]](c, "exclusions")
    } yield StemRange(stem, exclusions)
  }

  implicit lazy val decodeStemValue: Decoder[StemValue] =
    Decoder[IRI].map(iri => IRIStem(iri)).or(
      Decoder[Wildcard].map(w => w))

  implicit lazy val decodeWildcard: Decoder[Wildcard] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "Wildcard")
    } yield Wildcard()
  }

  implicit lazy val decodeTripleConstraint: Decoder[TripleConstraint] = Decoder.instance { c =>
    for {
      _ <- fixedFieldValue(c, "type", "TripleConstraint")
      inverse <- optFieldDecode[Boolean](c, "inverse")
      negated <- optFieldDecode[Boolean](c, "negated")
      predicate <- fieldDecode[IRI](c, "predicate")
      valueExpr <- optFieldDecode[ShapeExpr](c, "valueExpr")
      min <- optFieldDecode[Int](c, "min")
      max <- optFieldDecode[Max](c, "max")
      semActs <- optFieldDecode[List[SemAct]](c, "semActs")
      annotations <- optFieldDecode[List[Annotation]](c, "annotations")
    } yield TripleConstraint(inverse, negated, predicate, valueExpr, min, max, semActs, annotations)
  }

  // Utils...

  def fixedFieldValue(c: HCursor, name: String, value: String): Decoder.Result[String] =
    c.downField(name).as[String].flatMap(v =>
      if (v == value)
        Xor.right(v)
      else
        Xor.left(DecodingFailure(s"Required $value for field $name but got $v", Nil)))

  def fieldDecode[A: Decoder](c: HCursor, name: String): Decoder.Result[A] =
    c.downField(name).as[A]

  def optFieldDecode[A: Decoder](c: HCursor, name: String): Decoder.Result[Option[A]] = {
    val x = c.downField(name)
    if (x.succeeded) x.as[A].map(Some(_))
    else Xor.right(None)
  }

  def optFieldDecodeMap[A: KeyDecoder, B: Decoder](c: HCursor, name: String): Decoder.Result[Option[Map[A, B]]] = {
    val x = c.downField(name)
    if (x.succeeded) x.as[Map[A, B]].map(Some(_))
    else Xor.right(None)
  }

  def parsePrefix(str: String): Xor[String, Prefix] =
    str match {
      case prefixRegex(p) => Prefix(p).right
      case _              => Xor.Left(s"$str doesn't match prefix regex $prefixRegex")
    }

  def parseShapeLabel(str: String): Xor[String, ShapeLabel] = {
    str match {
      // Be careful with the order...
      case bNodeRegex(bNodeId) => BNodeLabel(BNodeId(bNodeId)).right
      case iriRegex(i)         => parseIRI(i).map(iri => IRILabel(iri))
      case _                   => Xor.left(s"$str doesn't match IRI or BNode")
    }
  }

  def parseLang(str: String): Xor[String, LangString] =
    str match {
      case langRegex(s, l) => LangString(s, l).right
      case _               => Xor.Left(s"$str doesn't match IRI regex $iriRegex")
    }

  def parseIRI(str: String): Xor[String, IRI] =
    str match {
      case iriRegex(i) => // TODO: Substitute by IRI.fromString(i)
        try {
          IRI(i).right
        } catch {
           case _: URISyntaxException =>
             Xor.Left(s"$str doesn't have the syntax of an URI")
        }
      case _ =>
        Xor.Left(s"$str doesn't match IRI regex $iriRegex")
    }


  lazy val prefixRegex: Regex = "^(.*)$".r // PN_PREFIX_STR.r
  lazy val iriRegex: Regex = "^(.*)$".r
  lazy val bNodeRegex: Regex = "^_:(.*)$".r
  lazy val stringRegex: Regex = "^\"(.*)\"$".r
  lazy val langRegex: Regex = "^\"(.*)\"@(.*)$".r
  lazy val datatypeRegex: Regex = "^\"(.[^\"]*)\"\\^\\^(.*)$".r

  /*
def startMiddleAltRep_Str(start: String, middleAlt: String, repEnd: String): String = {
    "(" + start + "(((" + repEnd + "|" + middleAlt + ")*(" + repEnd + "))?)" + ")"
}

lazy val PN_PREFIX_STR = startMiddleAltRep_Str(PN_CHARS_BASE, "\\.", PN_CHARS_STR)
lazy val PN_CHARS_STR = PN_CHARS_U + """|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040]"""
lazy val PN_CHARS_U = PN_CHARS_BASE + "|_"
lazy val PN_CHARS_BASE =
    """[a-zA-Z\u00C0-\u00D6\u00D8-\u00F6""" +
      """\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF""" +
      """\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF""" +
      """\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD""" +
      """\x{10000}-\x{EFFFF}]"""
*/
}
