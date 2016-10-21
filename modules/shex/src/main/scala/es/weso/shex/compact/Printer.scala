package es.weso.shex.compact
import scala.text._
import Document._
import es.weso.shex._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import annotation.tailrec
import es.weso.rdf._

/**
 * Convert Abstract syntax to ShEx Compact syntax
 */
object Printer {

  type Doc = Document

  def print(schema: Schema): String = {
    val doc = schemaDoc(schema)
    val writer = new java.io.StringWriter
    doc.format(1, writer)
    writer.toString
  }

  def comb(d1:Doc,d2:Doc): Doc =
    if (d1 == none) d2
    else d1 :/: d2

  def schemaDoc(s: Schema): Document = {
    comb(prefixesDoc(s.prefixes),
    comb(baseDoc(s.base),
    comb(startActsDoc(s.prefixMap)(s.startActs),
    comb(startDoc(s.prefixMap)(s.start),
          shapesDoc(s.shapes, s.prefixMap)))))
  }

  def prefixesDoc(ps: Option[PrefixMap]): Document =
    ps match {
      case None => empty
      case Some(pm) =>
        mapDocWithSeps(pm.pm,
          text("prefix")::space,space,newline,
          prefixDoc,
          unqualifiedIriDoc
      )
    }

  def prefixDoc(p: Prefix): Document =
    str(p.str)

  def unqualifiedIriDoc(iri: IRI): Document =
    str("<") :: str(iri.str) :: str(">")

  def baseDoc(b: Option[IRI]): Document =
    optDoc(b, (iri: IRI) =>
      str("base") :: space :: unqualifiedIriDoc(iri)
    )

  def startActsDoc(pm: PrefixMap)(s: Option[List[SemAct]]): Document =
    optDoc(s,semActsDoc(pm))

  def semActsDoc(pm: PrefixMap)(ls: List[SemAct]): Doc =
    listDocSep(ls,semActDoc(pm), newline)

  def semActDoc(pm: PrefixMap)(s: SemAct): Doc =
    str("%") :: iriDoc(pm)(s.name) :: s.code.fold(str("%"))(codeDoc(_))

  def codeDoc(c: String): Doc =
    str("{") :: str(c) :: str("%}")

  def startDoc(pm:PrefixMap)(s: Option[ShapeExpr]): Document =
    optDoc(s, (v: ShapeExpr) =>
      str("start") :: space :: eq :: space :: shapeExprDoc(pm)(v)
    )

  def shapesDoc(
    s: Option[Map[ShapeLabel, ShapeExpr]],
    pm: PrefixMap
  ): Document =
    optDoc(s, (shapesMap: Map[ShapeLabel,ShapeExpr]) =>
      mapDocWithSeps(shapesMap,
          none,space, newline,
          shapeLabelDoc(pm),
          shapeExprDoc(pm))
    )

  def shapeLabelDoc(pm: PrefixMap)(l: ShapeLabel): Document =
    l match {
      case IRILabel(iri) => str(pm.qualify(iri))
      case BNodeLabel(b) => str(b.toString)
    }

  def iriDoc(pm: PrefixMap)(iri: IRI): Doc =
    str(pm.qualify(iri))

  def shapeExprDoc(pm: PrefixMap)(se: ShapeExpr): Document =
    se match {
      case ShapeOr(es) =>
        listDocIntersperse(es,shapeExprDoc(pm),keyword("OR"))
      case ShapeAnd(es) =>
        listDocIntersperse(es,shapeExprDoc(pm),keyword("AND"))
      case ShapeNot(e) =>
        keyword("NOT") :: shapeExprDoc(pm)(e)
      case nc: NodeConstraint =>
        nodeConstraintDoc(pm)(nc)
      case s: Shape =>
        shapeDoc(pm)(s)
      case ShapeRef(r) =>
        str("@") :: shapeLabelDoc(pm)(r)
      case ShapeExternal() =>
        str("EXTERNAL")
    }

  def nodeConstraintDoc(pm: PrefixMap)(nc: NodeConstraint): Document =
    optDoc(nc.nodeKind,nodeKindDoc) ::
    optDoc(nc.datatype, datatypeDoc(pm)) ::
    listDocSep(nc.xsFacets, xsFacetDoc, space) ::
    optDoc(nc.values, valueSetDoc(pm))

  def nodeKindDoc(nc: NodeKind): Document =
    nc match {
      case IRIKind => keyword("IRI")
      case BNodeKind => keyword("BNode")
      case NonLiteralKind => keyword("NonLiteral")
      case LiteralKind => keyword("Literal")
    }

  def datatypeDoc(pm: PrefixMap)(d: IRI): Document =
    iriDoc(pm)(d)

  def xsFacetDoc(f: XsFacet): Document =
    f match {
      case Length(v) => f.fieldName :: space :: text(v.toString)
      case MinLength(v) => f.fieldName :: space :: text(v.toString)
      case MaxLength(v) => f.fieldName :: space :: text(v.toString)
      case Pattern(p) => f.fieldName :: space :: stringDoc(p)
      case MinInclusive(n) => f.fieldName :: space :: numericDoc(n)
      case MaxInclusive(n) => f.fieldName :: space :: numericDoc(n)
      case MinExclusive(n) => f.fieldName :: space :: numericDoc(n)
      case MaxExclusive(n) => f.fieldName :: space :: numericDoc(n)
      case TotalDigits(n) => f.fieldName :: space :: text(n.toString)
      case FractionDigits(n) => f.fieldName :: space :: text(n.toString)
    }

  def numericDoc(n: NumericLiteral): Document =
    str("TODO: NumericLiteral")

  def valueSetDoc(pm: PrefixMap)(vs: List[ValueSetValue]): Document =
    keyword("[") ::
    listDocSep(vs, valueDoc(pm), space) ::
    keyword("]")

  // TODO
  def escape(str: String): String = {
    str.map(escapeChar(_)).flatten.mkString
  }

  def escapeChar(c: Char): List[Char] =
    c match {
      // case '\"' => List('\\','\"')
      case _ => List(c)
    }

  def stringDoc(str: String): Document =
    text("\"") :: text(escape(str)) :: text("\"")

  def valueDoc(pm: PrefixMap)(v: ValueSetValue): Document =
    v match {
      case IRIValue(iri) => iriDoc(pm)(iri)
      case StringValue(str) => stringDoc(str)
      case d: DatatypeString => datatypeStringDoc(pm)(d)
      case LangString(s,l) => stringDoc(s) :: str("@") :: str(l)
      case Stem(iri) => iriDoc(pm)(iri) :: str("~")
      case StemRange(stem,exclusions) => str("TODO: StemRange")
    }

  def datatypeStringDoc(pm: PrefixMap)(dt: DatatypeString): Document =
    dt.iri match {
      case `xsd_boolean` => booleanDoc(dt.s)
      case `xsd_integer` => integerDoc(dt.s)
      case `xsd_decimal` => decimalDoc(dt.s)
      case `xsd_double` => doubleDoc(dt.s)
      case _ => stringDoc(dt.s) :: str("^^") :: iriDoc(pm)(dt.iri)
    }

  def booleanDoc(s: String): Document = text(s)

  def integerDoc(s: String): Document = text(s.toString)

  def decimalDoc(s: String): Document = text(s.toString)

  def doubleDoc(s: String): Document = text(s.toString)


  def shapeDoc(pm: PrefixMap)(s: Shape): Document = {
    optDocConst(s.virtual,keyword("VIRTUAL")) ::
    optDocConst(s.closed,keyword("CLOSED")) ::
    optDoc(s.extra,extraDoc(pm))::
    optDoc(s.expression,
      (te: TripleExpr) =>
        text("{") :: tripleExprDoc(pm)(te) :: text("}"):: newline
      ) ::
    optDoc(s.semActs,semActsDoc(pm))
  }

  def extraDoc(pm: PrefixMap)(ls: List[IRI]): Document =
    keyword("EXTRA") :: listDocSep(ls, iriDoc(pm), space)

  def tripleExprDoc(pm: PrefixMap)(t: TripleExpr): Document =
    t match {
      case e: EachOf => eachOfDoc(pm)(e)
      case e: SomeOf => someOfDoc(pm)(e)
      case Inclusion(l) => keyword("&") :: shapeLabelDoc(pm)(l)
      case t: TripleConstraint => tripleConstraintDoc(pm)(t)
    }

  def eachOfDoc(pm: PrefixMap)(e: EachOf): Document =
    listDocIntersperse(e.expressions,tripleExprDoc(pm),keyword(";")) ::
    cardinalityDoc(e.optMin,e.optMax) ::
    optDoc(e.semActs,semActsDoc(pm)) ::
    optDoc(e.annotations, annotationsDoc(pm))

  def someOfDoc(pm: PrefixMap)(e: SomeOf): Document =
      listDocIntersperse(e.expressions,tripleExprDoc(pm),keyword("|")) ::
      cardinalityDoc(e.optMin,e.optMax) ::
      optDoc(e.semActs,semActsDoc(pm)) ::
      optDoc(e.annotations, annotationsDoc(pm))

  def tripleConstraintDoc(pm: PrefixMap)(t: TripleConstraint): Document =
    optDocConst(t.optInverse, str("^")) ::
    optDocConst(t.optNegated, str("!")) ::
    iriDoc(pm)(t.predicate) :: space ::
    optDoc(t.valueExpr, shapeExprDoc(pm)) ::
    cardinalityDoc(t.optMin,t.optMax) ::
    optDoc(t.semActs,semActsDoc(pm)) ::
    optDoc(t.annotations, annotationsDoc(pm))

  def annotationsDoc(pm: PrefixMap)(as: List[Annotation]): Document =
    str("todo: Annotations")

  def cardinalityDoc(min: Option[Int], max: Option[Max]): Document =
    str(s" {${min.getOrElse(1)},${max.getOrElse(IntMax(1)).show}}")

  def listDocSep[A](
    xs: Seq[A],
    toDoc: A => Document,
    sep: Document
  ): Document = xs.foldLeft(empty: Document)(
      (d: Document, x: A) => d :: sep :: toDoc(x))

  def listDocIntersperse[A](s: Seq[A],
    toDoc: A => Document,
    sep: Document
  ): Document =
    flatten(intersperse(sep,s.toList.map(toDoc(_))))

  def flatten(ls: List[Doc]): Doc =
    ls.foldLeft(empty:Document)(
      (d1,d2) => d1 :: d2
    )

  def pairDoc[A, B](doc1: A => Doc,
    sep: Doc,
    doc2: B => Doc)(pair: (A, B)): Document = {
    doc1(pair._1) :: sep :: doc2(pair._2)
  }

  def mapDocWithSeps[A, B](
    m: Map[A, B],
    beforeKey: Document,
    betweenKeyValue: Document,
    afterValue: Document,
    toDocKey: A => Document,
    toDocValue: B => Document
  ): Document = {
    listDocSep(
      m.toList,
      (p: (A,B)) => beforeKey ::
        pairDoc(toDocKey, betweenKeyValue, toDocValue)(p),
      afterValue
    )
  }

  def optDoc[A](x: Option[A], f: A => Doc): Doc =
    x.fold(none)(f(_))

  def optDocConst[A](x: Option[A], c: Doc): Doc =
    x.fold(none)(_ => c)

  def eq = str("=")

  def space: Document = str(" ")
  def keyword(s: String): Document = str(s) :: space
  def none: Doc = empty
  def newline: Doc = break
  def str(str: String): Document = text(str)

  // TODO: search if this method is already defined elsewhere...
  def intersperse[A](a: A, xs: List[A]): List[A] = {
    @tailrec
    def intersperse0(accum: List[A], rest: List[A]): List[A] = rest match {
      case Nil => accum
      case x :: Nil => x :: accum
      case h :: t => intersperse0(a :: h :: accum, t)
    }
    intersperse0(Nil, xs).reverse
  }
}
