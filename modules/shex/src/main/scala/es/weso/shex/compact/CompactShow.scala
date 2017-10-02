package es.weso.shex.compact

import es.weso.rdf.PREFIXES._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.utils.SeqUtils.intersperse

import scala.text.Document._
import scala.text._

/**
 * Convert Abstract syntax to ShEx Compact syntax
 */
object CompactShow {

  type Doc = Document

  def showSchema(schema: Schema): String = {
    doc2String(schemaDoc(schema))
  }

  def showShapeExpr(shapeExpr: ShapeExpr, pm: PrefixMap): String = {
    doc2String(shapeExprDoc(pm)(shapeExpr))
  }

  def showValueSet(values: List[ValueSetValue], pm: PrefixMap): String = {
    doc2String(valueSetDoc(pm)(values))
  }

  def doc2String(doc: Doc): String = {
    val writer = new java.io.StringWriter
    doc.format(1, writer)
    writer.toString
  }

  private def comb(d1: Doc, d2: Doc): Doc =
    if (d1 == none) d2
    else d1 :/: d2

  private def schemaDoc(s: Schema): Document = {
    comb(
      prefixesDoc(s.prefixes),
      comb(
        baseDoc(s.base),
        comb(
          startActsDoc(s.prefixMap)(s.startActs),
          comb(
            startDoc(s.prefixMap)(s.start),
            optShapesDoc(s.shapes, s.prefixMap)))))
  }

  private def prefixesDoc(ps: Option[PrefixMap]): Document =
    ps match {
      case None => empty
      case Some(pm) =>
        mapDocWithSeps(
          pm.pm,
          text("prefix") :: space, space, newline,
          prefixDoc,
          unqualifiedIriDoc)
    }

  private def prefixDoc(p: Prefix): Document =
    str(p.str + ":")

  private def unqualifiedIriDoc(iri: IRI): Document =
    str("<") :: str(iri.str) :: str(">")

  private def baseDoc(b: Option[IRI]): Document =
    optDoc(b, (iri: IRI) =>
      str("base") :: space :: unqualifiedIriDoc(iri))

  private def startActsDoc(pm: PrefixMap)(s: Option[List[SemAct]]): Document =
    optDoc(s, semActsDoc(pm))

  private def semActsDoc(pm: PrefixMap)(ls: List[SemAct]): Doc =
    listDocSep(ls, semActDoc(pm), newline)

  private def semActDoc(pm: PrefixMap)(s: SemAct): Doc =
    str("%") :: iriDoc(pm)(s.name) :: s.code.fold(str("%"))(codeDoc(_))

  private def codeDoc(c: String): Doc =
    str("{") :: str(c) :: str("%}")

  private def startDoc(pm: PrefixMap)(s: Option[ShapeExpr]): Document =
    optDoc(s, (v: ShapeExpr) =>
      str("start") :: space :: eq :: space :: shapeExprDoc(pm)(v))

  private def optShapesDoc(
    s: Option[List[ShapeExpr]],
    pm: PrefixMap): Document =
    optDoc(s, shapeExprsDoc(pm))

  private def shapeExprsDoc(pm: PrefixMap)(shapes: List[ShapeExpr]): Document =
    listDocSep(shapes, shapeExprDoc(pm), newline)

  private def shapeLabelDoc(pm: PrefixMap)(l: ShapeLabel): Document =
    l match {
      case IRILabel(iri) => str(pm.qualify(iri))
      case BNodeLabel(b) => str(b.toString)
    }

  private def iriDoc(pm: PrefixMap)(iri: IRI): Doc =
    str(pm.qualify(iri))

  def idDoc(id: Option[ShapeLabel], pm: PrefixMap): Document = id match {
    case None => empty
    case Some(label) => shapeLabelDoc(pm)(label)
  }

  private def shapeExprDoc(pm: PrefixMap)(se: ShapeExpr): Document =
    se match {
      // TODO...review ids generation...
      case ShapeOr(id, es) =>
        idDoc(id, pm) :: space ::
          listDocIntersperse(es, shapeExprDoc(pm), keyword("OR"))
      case ShapeAnd(id, es) =>
        idDoc(id, pm) :: space ::
          listDocIntersperse(es, shapeExprDoc(pm), keyword("AND"))
      case ShapeNot(id, e) =>
        idDoc(id, pm) :: space ::
          keyword("NOT") :: shapeExprDoc(pm)(e)
      case nc: NodeConstraint =>
        nodeConstraintDoc(pm)(nc)
      case s: Shape =>
        shapeDoc(pm)(s)
      case ShapeRef(r) =>
        str("@") :: shapeLabelDoc(pm)(r)
      case ShapeExternal(id) =>
        idDoc(id, pm) :: space ::
          str("EXTERNAL")
    }

  private def nodeConstraintDoc(pm: PrefixMap)(nc: NodeConstraint): Document =
    if (nc == NodeConstraint.empty) {
      dot
    } else
      optDoc(nc.nodeKind, nodeKindDoc) ::
        optDoc(nc.datatype, datatypeDoc(pm)) ::
        listDocSep(nc.xsFacets, xsFacetDoc, space) ::
        optDoc(nc.values, valueSetDoc(pm))

  private def nodeKindDoc(nc: NodeKind): Document =
    nc match {
      case IRIKind => keyword("IRI")
      case BNodeKind => keyword("BNode")
      case NonLiteralKind => keyword("NonLiteral")
      case LiteralKind => keyword("Literal")
    }

  private def datatypeDoc(pm: PrefixMap)(d: IRI): Document =
    iriDoc(pm)(d)

  private def xsFacetDoc(f: XsFacet): Document =
    f match {
      case Length(v) => f.fieldName :: space :: text(v.toString)
      case MinLength(v) => f.fieldName :: space :: text(v.toString)
      case MaxLength(v) => f.fieldName :: space :: text(v.toString)
      case Pattern(p, flags) => text("~/") :: text(p) :: text("/") :: text(flags.getOrElse(""))
      case MinInclusive(n) => f.fieldName :: space :: numericDoc(n)
      case MaxInclusive(n) => f.fieldName :: space :: numericDoc(n)
      case MinExclusive(n) => f.fieldName :: space :: numericDoc(n)
      case MaxExclusive(n) => f.fieldName :: space :: numericDoc(n)
      case TotalDigits(n) => f.fieldName :: space :: text(n.toString)
      case FractionDigits(n) => f.fieldName :: space :: text(n.toString)
    }

  private def numericDoc(n: NumericLiteral): Document = n match {
    case NumericInt(n) => integerDoc(n.toString)
    case NumericDouble(d) => doubleDoc(d.toString)
    case NumericDecimal(d) => decimalDoc(d.toString)
  }

  private def valueSetDoc(pm: PrefixMap)(vs: List[ValueSetValue]): Document =
    keyword("[") ::
      listDocSep(vs, valueDoc(pm), space) ::
      keyword("]")

  // TODO
  private def escape(str: String): String = {
    str.map(escapeChar(_)).flatten.mkString
  }

  private def escapeChar(c: Char): List[Char] =
    c match {
      // case '\"' => List('\\','\"')
      case _ => List(c)
    }

  private def stringDoc(str: String): Document =
    text("\"") :: text(escape(str)) :: text("\"")

  private def valueDoc(pm: PrefixMap)(v: ValueSetValue): Document =
    v match {
      case IRIValue(iri) => iriDoc(pm)(iri)
      case StringValue(str) => stringDoc(str)
      case d: DatatypeString => datatypeStringDoc(pm)(d)
      case LangString(s, l) => stringDoc(s) :: str("@") :: str(l)
      case Stem(iri) => iriDoc(pm)(iri) :: str("~")
      case StemRange(stem, exclusions) => str("TODO: StemRange")
    }

  private def datatypeStringDoc(pm: PrefixMap)(dt: DatatypeString): Document =
    dt.iri match {
      case `xsd_boolean` => booleanDoc(dt.s)
      case `xsd_integer` => integerDoc(dt.s)
      case `xsd_decimal` => decimalDoc(dt.s)
      case `xsd_double` => doubleDoc(dt.s)
      case _ => stringDoc(dt.s) :: str("^^") :: iriDoc(pm)(dt.iri)
    }

  private def booleanDoc(s: String): Document = text(s)

  private def integerDoc(s: String): Document = text(s.toString)

  private def decimalDoc(s: String): Document = text(s.toString)

  private def doubleDoc(s: String): Document = text(s.toString)

  private def maybeClosed(closed: Boolean): Document =
    if (closed) { keyword("CLOSED") } else empty

  private def shapeDoc(pm: PrefixMap)(s: Shape): Document = {
    idDoc(s.id, pm) :: space ::
      optDocConst(s.virtual, keyword("VIRTUAL")) ::
      maybeClosed(s.isClosed) ::
      optDoc(s.extra, extraDoc(pm)) ::
      optDoc(
        s.expression,
        (te: TripleExpr) =>
          text("{") :: tripleExprDoc(pm)(te) :: text("}") :: newline) ::
        optDoc(s.semActs, semActsDoc(pm))
  }

  private def extraDoc(pm: PrefixMap)(ls: List[IRI]): Document =
    keyword("EXTRA") :: listDocSep(ls, iriDoc(pm), space)

  private def tripleExprDoc(pm: PrefixMap)(t: TripleExpr): Document =
    t match {
      case e: EachOf => eachOfDoc(pm)(e)
      case e: OneOf => someOfDoc(pm)(e)
      case Inclusion(l) => keyword("&") :: shapeLabelDoc(pm)(l)
      case t: TripleConstraint => tripleConstraintDoc(pm)(t)
    }

  private def eachOfDoc(pm: PrefixMap)(e: EachOf): Document = {
    val kernel = if (Cardinality.isDefault(e.min, e.max)) {
      listDocIntersperse(e.expressions, tripleExprDoc(pm), keyword(";"))
    } else {
      openParen ::
        listDocIntersperse(e.expressions, tripleExprDoc(pm), keyword(";")) ::
        closeParen ::
        cardinalityDoc(e.optMin, e.optMax)
    }
    kernel ::
      optDoc(e.semActs, semActsDoc(pm)) ::
      optDoc(e.annotations, annotationsDoc(pm))
  }
  private def someOfDoc(pm: PrefixMap)(e: OneOf): Document = {
    val kernel = if (Cardinality.isDefault(e.min, e.max)) {
      listDocIntersperse(e.expressions, tripleExprDoc(pm), keyword("|"))
    } else {
      openParen ::
        listDocIntersperse(e.expressions, tripleExprDoc(pm), keyword("|")) ::
        closeParen ::
        cardinalityDoc(e.optMin, e.optMax)
    }
    kernel ::
      optDoc(e.semActs, semActsDoc(pm)) ::
      optDoc(e.annotations, annotationsDoc(pm))
  }

  private def tripleConstraintDoc(pm: PrefixMap)(t: TripleConstraint): Document =
    optDocConst(t.optInverse, str("^")) ::
      optDocConst(t.optNegated, str("!")) ::
      iriDoc(pm)(t.predicate) :: space ::
      optDoc(t.valueExpr, shapeExprDoc(pm)) ::
      (if (Cardinality.isDefault(t.min, t.max)) none
      else cardinalityDoc(t.optMin, t.optMax)) ::
      optDoc(t.semActs, semActsDoc(pm)) ::
      optDoc(t.annotations, annotationsDoc(pm))

  private def annotationsDoc(pm: PrefixMap)(as: List[Annotation]): Document =
    str("todo: Annotations")

  private def cardinalityDoc(min: Option[Int], max: Option[Max]): Document =
    str(s" {${min.getOrElse(1)},${max.getOrElse(IntMax(1)).show}}")

  private def listDocSep[A](
    xs: Seq[A],
    toDoc: A => Document,
    sep: Document): Document = xs.foldLeft(empty: Document)(
    (d: Document, x: A) => d :: sep :: toDoc(x))

  private def listDocIntersperse[A](
    s: Seq[A],
    toDoc: A => Document,
    sep: Document): Document =
    flatten(intersperse(sep, s.toList.map(toDoc(_))))

  private def flatten(ls: Seq[Doc]): Doc =
    ls.foldLeft(empty: Document)(
      (d1, d2) => d1 :: d2)

  private def pairDoc[A, B](
    doc1: A => Doc,
    sep: Doc,
    doc2: B => Doc)(pair: (A, B)): Document = {
    doc1(pair._1) :: sep :: doc2(pair._2)
  }

  private def mapDocWithSeps[A, B](
    m: Map[A, B],
    beforeKey: Document,
    betweenKeyValue: Document,
    afterValue: Document,
    toDocKey: A => Document,
    toDocValue: B => Document): Document = {
    listDocSep(
      m.toList,
      (p: (A, B)) => beforeKey ::
        pairDoc(toDocKey, betweenKeyValue, toDocValue)(p),
      afterValue)
  }

  private def optDoc[A](x: Option[A], f: A => Doc): Doc =
    x.fold(none)(f(_))

  private def optDocConst[A](x: Option[A], c: Doc): Doc =
    x.fold(none)(_ => c)

  private def eq = str("=")

  private def space: Document = str(" ")

  private def keyword(s: String): Document = str(s) :: space

  private def none: Doc = empty

  private def newline: Doc = break

  private def dot: Document = text(".")
  private def openParen: Document = text("(")
  private def closeParen: Document = text(")")
  private def str(str: String): Document = text(str)

}
