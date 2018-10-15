package es.weso.shex.compact

import es.weso.rdf.PREFIXES._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.utils.SeqUtils.intersperse
import es.weso.document._
import es.weso.document.Document._
import es.weso.rdf.operations.Comparisons._



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

  private def schemaDoc(s: Schema): Doc = {
    comb(
      prefixesDoc(s.prefixes),
      comb(
        baseDoc(s.base),
        comb(importsDoc(s.prefixMap, s.imports),
        comb(
          startActsDoc(s.prefixMap)(s.startActs),
          comb(
            startDoc(s.prefixMap)(s.start),
            optShapesDoc(s.shapes, s.prefixMap))))))
  }

  private def prefixesDoc(ps: Option[PrefixMap]): Doc =
    ps match {
      case None => empty
      case Some(pm) =>
        mapDocWithSeps(
          pm.pm,
          text("prefix") :: space, space, newline,
          prefixDoc,
          unqualifiedIriDoc)
    }

  private def importsDoc(pm: PrefixMap, iris: List[IRI]): Doc =
    listDocSep(iris, importDoc(pm), newline)

  private def importDoc(pm: PrefixMap)(iri: IRI): Doc =
    text("import") :: space :: iriDoc(pm)(iri)

  private def prefixDoc(p: Prefix): Doc =
    str(p.str + ":")

  private def unqualifiedIriDoc(iri: IRI): Doc =
    str("<") :: str(iri.str) :: str(">")

  private def baseDoc(b: Option[IRI]): Doc =
    optDoc(b, (iri: IRI) =>
      str("base") :: space :: unqualifiedIriDoc(iri))

  private def startActsDoc(pm: PrefixMap)(s: Option[List[SemAct]]): Doc =
    optDoc(s, semActsDoc(pm))

  private def semActsDoc(pm: PrefixMap)(ls: List[SemAct]): Doc =
    listDocSep(ls, semActDoc(pm), newline)

  private def semActDoc(pm: PrefixMap)(s: SemAct): Doc =
    str("%") :: iriDoc(pm)(s.name) :: s.code.fold(str("%"))(codeDoc(_))

  private def codeDoc(c: String): Doc =
    str("{") :: str(c) :: str("%}")

  private def startDoc(pm: PrefixMap)(s: Option[ShapeExpr]): Doc =
    optDoc(s, (v: ShapeExpr) =>
      str("start") :: space :: eq :: space :: shapeExprDoc(pm)(v))

  private def optShapesDoc(
    s: Option[List[ShapeExpr]],
    pm: PrefixMap): Document =
    optDoc(s, shapeExprsDoc(pm))

  private def shapeExprsDoc(pm: PrefixMap)(shapes: List[ShapeExpr]): Doc =
    listDocSep(shapes, shapeExprDoc(pm), newline)

  private def shapeLabelDoc(pm: PrefixMap)(l: ShapeLabel): Doc =
    l match {
      case IRILabel(iri) => str(pm.qualify(iri))
      case BNodeLabel(b) => str(b.toString)
    }

  private def iriDoc(pm: PrefixMap)(iri: IRI): Doc =
    str(pm.qualify(iri))

  def idDoc(id: Option[ShapeLabel], pm: PrefixMap): Doc = id match {
    case None => empty
    case Some(label) => shapeLabelDoc(pm)(label)
  }

  private def shapeExprDoc(pm: PrefixMap)(se: ShapeExpr): Doc =
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
        idDoc(nc.id, pm) :: space ::
          nodeConstraintDoc(pm)(nc)
      case s: Shape =>
        idDoc(s.id, pm) :: space ::
          shapeDoc(pm)(s)
      case ShapeRef(r) =>
        str("@") :: shapeLabelDoc(pm)(r)
      case ShapeExternal(id) =>
        idDoc(id, pm) :: space ::
          str("EXTERNAL")
    }

  private def nodeConstraintDoc(pm: PrefixMap)(nc: NodeConstraint): Doc =
    if (nc == NodeConstraint.empty) {
      dot
    } else
      optDoc(nc.nodeKind, nodeKindDoc) ::
        optDoc(nc.datatype, datatypeDoc(pm)) ::
        listDocSep(nc.xsFacets, xsFacetDoc, space) ::
        optDoc(nc.values, valueSetDoc(pm))

  private def nodeKindDoc(nc: NodeKind): Doc =
    nc match {
      case IRIKind => keyword("IRI")
      case BNodeKind => keyword("BNode")
      case NonLiteralKind => keyword("NonLiteral")
      case LiteralKind => keyword("Literal")
    }

  private def datatypeDoc(pm: PrefixMap)(d: IRI): Doc =
    iriDoc(pm)(d)

  private def xsFacetDoc(f: XsFacet): Doc =
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

  private def numericDoc(n: NumericLiteral): Doc = n match {
    case NumericInt(_,str) => integerDoc(str)
    case NumericDouble(_,str) => doubleDoc(str)
    case NumericDecimal(_,str) => decimalDoc(str)
  }

  private def valueSetDoc(pm: PrefixMap)(vs: List[ValueSetValue]): Doc =
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

  private def stringDoc(str: String): Doc =
    text("\"") :: text(escape(str)) :: text("\"")

  private def valueDoc(pm: PrefixMap)(v: ValueSetValue): Doc =
    v match {
      case IRIValue(iri) => iriDoc(pm)(iri)
      case StringValue(str) => stringDoc(str)
      case d: DatatypeString => datatypeStringDoc(pm)(d)
      case LangString(s, l) => stringDoc(s) :: str("@") :: str(l.lang)
      case IRIStem(iri) => iriDoc(pm)(iri) :: str("~")
      case IRIStemRange(_, _) => str("TODO: StemRange")
      case LanguageStem(s) => str(s"@$s")
      case _ => str(s"Unimplemented show of $v")
    }

  private def datatypeStringDoc(pm: PrefixMap)(dt: DatatypeString): Doc =
    dt.iri match {
      case `xsd_boolean` => booleanDoc(dt.s)
      case `xsd_integer` => integerDoc(dt.s)
      case `xsd_decimal` => decimalDoc(dt.s)
      case `xsd_double` => doubleDoc(dt.s)
      case _ => stringDoc(dt.s) :: str("^^") :: iriDoc(pm)(dt.iri)
    }

  private def booleanDoc(s: String): Doc = text(s)

  private def integerDoc(s: String): Doc = text(s.toString)

  private def decimalDoc(s: String): Doc = text(s.toString)

  private def doubleDoc(s: String): Doc = text(s.toString)

  private def maybeClosed(closed: Boolean): Doc =
    if (closed) { keyword("CLOSED") } else empty

  private def shapeDoc(pm: PrefixMap)(s: Shape): Doc = {
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

  private def extraDoc(pm: PrefixMap)(ls: List[IRI]): Doc =
    keyword("EXTRA") :: listDocSep(ls, iriDoc(pm), space)

  private def tripleExprDoc(pm: PrefixMap)(t: TripleExpr): Doc =
    t match {
      case e: EachOf => eachOfDoc(pm)(e)
      case e: OneOf => someOfDoc(pm)(e)
      case Inclusion(l) => keyword("&") :: shapeLabelDoc(pm)(l)
      case t: TripleConstraint => tripleConstraintDoc(pm)(t)
      case e: Expr => exprDoc(pm)(e)
    }

  private def eachOfDoc(pm: PrefixMap)(e: EachOf): Doc = {
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
  private def someOfDoc(pm: PrefixMap)(e: OneOf): Doc = {
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

  private def tripleConstraintDoc(pm: PrefixMap)(t: TripleConstraint): Doc =
    optDocConst(t.optInverse, str("^")) ::
      optDocConst(t.optNegated, str("!")) ::
      iriDoc(pm)(t.predicate) :: space ::
      optDoc(t.valueExpr, shapeExprDoc(pm)) ::
      (if (Cardinality.isDefault(t.min, t.max)) none
      else cardinalityDoc(t.optMin, t.optMax)) ::
      optDoc(t.semActs, semActsDoc(pm)) ::
      optDoc(t.annotations, annotationsDoc(pm))

  private def annotationsDoc(pm: PrefixMap)(as: List[Annotation]): Doc =
    str(s"CompactShow/TODO: Annotations $as $pm")

  private def cardinalityDoc(min: Option[Int], max: Option[Max]): Doc =
    str(s" {${min.getOrElse(1)},${max.getOrElse(IntMax(1)).show}}")

  private def exprDoc(pm: PrefixMap)(e: Expr): Doc =
    str(s"CompactShow/TODO: Expressions $e")

  private def listDocSep[A](
    xs: Seq[A],
    toDoc: A => Doc,
    sep: Doc): Doc = xs.foldLeft(empty: Doc)(
    (d: Doc, x: A) => d :: sep :: toDoc(x))

  private def listDocIntersperse[A](
    s: Seq[A],
    toDoc: A => Doc,
    sep: Doc): Doc =
    flatten(intersperse(sep, s.toList.map(toDoc(_))))

  private def flatten(ls: Seq[Doc]): Doc =
    ls.foldLeft(empty: Doc)(
      (d1, d2) => d1 :: d2)

  private def pairDoc[A, B](
    doc1: A => Doc,
    sep: Doc,
    doc2: B => Doc)(pair: (A, B)): Doc = {
    doc1(pair._1) :: sep :: doc2(pair._2)
  }

  private def mapDocWithSeps[A, B](
    m: Map[A, B],
    beforeKey: Doc,
    betweenKeyValue: Doc,
    afterValue: Doc,
    toDocKey: A => Doc,
    toDocValue: B => Doc): Doc = {
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

  private def space: Doc = str(" ")

  private def keyword(s: String): Doc = str(s) :: space

  private def none: Doc = empty

  private def newline: Doc = break

  private def dot: Doc = text(".")
  private def openParen: Doc = text("(")
  private def closeParen: Doc = text(")")
  private def str(str: String): Doc = text(str)

}
