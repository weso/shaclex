package es.weso.shex.shexR

import es.weso.shex._
import PREFIXES._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.jena._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import org.apache.jena.rdf.model.Model

trait ShEx2RDF extends RDFSaver with LazyLogging {

  def serialize(shex: Schema, node: Option[IRI], format: String): Either[String,String] = {
    val rdf: RDFAsJenaModel = toRDF(shex, node, RDFAsJenaModel.empty)
    rdf.serialize(format)
  }

  def toRDF(s: Schema, node: Option[IRI], initial: RDFAsJenaModel): RDFAsJenaModel = {
    val result = schema(s, node).run(initial)
    result.value._1
  }

  def schema(s: Schema, id: Option[IRI]): RDFSaver[Unit] = {
    for {
      node <- makeId(id)
      _ <- addPrefixMap(s.prefixMap)
      _ <- addTriple(node, rdf_type, sx_Schema)
      _ <- maybeAddListContent(s.startActs, node, sx_startActs, semAct)
      _ <- maybeAddContent(s.start, node, sx_start, shapeExpr)
      _ <- maybeAddStarContent(s.shapes, node, sx_shapes, shapeExpr)
    } yield ()
  }

  def shapeExpr(e: ShapeExpr): RDFSaver[RDFNode] = e match {

    case ShapeAnd(id, shapeExprs) => for {
      node <- mkId(id)
      _ <- addTriple(node, rdf_type, sx_ShapeAnd)
      _ <- addListContent(shapeExprs, node, sx_expressions, shapeExpr)
    } yield node

    case ShapeOr(id, shapeExprs) => for {
      node <- mkId(id)
      _ <- addTriple(node, rdf_type, sx_ShapeOr)
      _ <- addListContent(shapeExprs, node, sx_expressions, shapeExpr)
    } yield node

    case ShapeNot(id, se) => for {
      node <- mkId(id)
      _ <- addTriple(node, rdf_type, sx_ShapeNot)
      _ <- addContent(se, node, sx_expression, shapeExpr)
    } yield node

    case NodeConstraint(id, nk, dt, facets, values) => for {
      shapeId <- mkId(id)
      _ <- addTriple(shapeId, rdf_type, sx_NodeConstraint)
      _ <- maybeAddContent(nk, shapeId, sx_nodeKind, nodeKind)
      _ <- maybeAddContent(dt, shapeId, sx_datatype, iri)
      _ <- facets.map(xsFacet(_, shapeId)).sequence
      _ <- maybeAddListContent(values, shapeId, sx_values, valueSetValue)
    } yield shapeId

    case Shape(id, virtual, closed, extra, expr, inherit, semActs,annotations) => for {
      shapeId <- mkId(id)
      _ <- addTriple(shapeId, rdf_type, sx_Shape)
      _ <- maybeAddContent(closed, shapeId, sx_closed, rdfBoolean)
      _ <- maybeAddStarContent(extra, shapeId, sx_extra, iri)
      _ <- maybeAddContent(expr, shapeId, sx_expression, tripleExpr)
      _ <- maybeAddListContent(semActs, shapeId, sx_semActs, semAct)
      _ <- maybeAddStarContent(annotations, shapeId, sx_annotation, annotation)
    } yield shapeId

    case ShapeExternal(id) => for {
      shapeId <- mkId(id)
      _ <- addTriple(shapeId, rdf_type, sx_ShapeExternal)
    } yield shapeId

    case ShapeRef(lbl) => label(lbl)
  }

  def xsFacet(facet: XsFacet, node: RDFNode): RDFSaver[Unit] = facet match {
    case Length(n) => addTriple(node, sx_length, IntegerLiteral(n))
    case MinLength(n) => addTriple(node, sx_minlength, IntegerLiteral(n))
    case MaxLength(n) => addTriple(node, sx_maxlength, IntegerLiteral(n))
    case Pattern(n, flags) => for {
      _ <- addTriple(node, sx_pattern, StringLiteral(n))
      _ <- maybeAddTriple(node, sx_flags, flags.map(StringLiteral(_)))
    } yield ()
    case MinInclusive(n) => addContent(n, node, sx_mininclusive, numericLiteral)
    case MinExclusive(n) => addContent(n, node, sx_minexclusive, numericLiteral)
    case MaxInclusive(n) => addContent(n, node, sx_maxinclusive, numericLiteral)
    case MaxExclusive(n) => addContent(n, node, sx_maxexclusive, numericLiteral)
    case FractionDigits(n) => addTriple(node, sx_fractiondigits, IntegerLiteral(n))
    case TotalDigits(n) => addTriple(node, sx_totaldigits, IntegerLiteral(n))
  }

  def numericLiteral(n: NumericLiteral): RDFSaver[RDFNode] = n match {
    case NumericInt(n) => ok(IntegerLiteral(n))
    case NumericDouble(n,_) => ok(DoubleLiteral(n))
    case NumericDecimal(n,_) => ok(DecimalLiteral(n))
  }

  def valueSetValue(x: ValueSetValue): RDFSaver[RDFNode] = x match {
    case IRIValue(iri) => ok(iri)
    case StringValue(s) => ok(StringLiteral(s))
    case DatatypeString(s, iri) => ok(DatatypeLiteral(s, iri))
    case LangString(s, lang) => ok(LangLiteral(s, lang))
    case Language(Lang(str)) => ok(StringLiteral(str))  // TODO: This one looks suspicious (it seems to have the same representation as a string)
    case s: IRIStem => iriStem(s)
    case i : IRIStemRange => iriStemRange(i)
    case s: LanguageStem => languageStem(s)
    case l: LanguageStemRange => languageStemRange(l)
    case l: LiteralStem => literalStem(l)
    case l: LiteralStemRange => literalStemRange(l)
  }


  def iriExclusion(iri: IRIExclusion): RDFSaver[RDFNode] = iri match {
    case IRIRefExclusion(iri) => ok(iri)
    case IRIStemExclusion(stem) => iriStem(stem)
  }

  def literalExclusion(le: LiteralExclusion): RDFSaver[RDFNode] = le match {
    case LiteralStringExclusion(str) => ok(StringLiteral(str))
    case LiteralStemExclusion(stem) => literalStem(stem)
  }

  def languageExclusion(le: LanguageExclusion): RDFSaver[RDFNode] = le match {
    case LanguageTagExclusion(Lang(str)) => ok(StringLiteral(str))
    case LanguageStemExclusion(stem) => languageStem(stem)
  }

  def iriStem(x: IRIStem): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, rdf_type, sx_IriStem)
    _ <- addTriple(node, sx_stem, DatatypeLiteral(x.stem.str, xsd_anyUri))
  } yield node

  def languageStem(x: LanguageStem): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, rdf_type, sx_LanguageStem)
    _ <- addTriple(node, sx_stem, StringLiteral(x.stem.lang))
  } yield node

  def literalStem(x: LiteralStem): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, rdf_type, sx_LiteralStem)
    _ <- addTriple(node, sx_stem, StringLiteral(x.stem))
  } yield node

  def iriStemRange(range: IRIStemRange): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, rdf_type, sx_IriStemRange)
    _ <- addContent(range.stem, node, sx_stem, iriStemRangeValue)
    _ <- maybeAddStarContent(range.exclusions, node, sx_exclusion, iriExclusion)
  } yield node

  def iriStemRangeValue(x: IRIStemRangeValue): RDFSaver[RDFNode] = x match {
    case IRIStemValueIRI(iri) => ok(DatatypeLiteral(iri.str, xsd_anyUri))
    case IRIStemWildcard() => for {
      node <- createBNode()
      _ <- addTriple(node, rdf_type, sx_Wildcard)
    } yield node
  }

  def literalStemRange(range: LiteralStemRange): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, rdf_type, sx_LiteralStemRange)
    _ <- addContent(range.stem, node, sx_stem, literalStemRangeValue)
    _ <- maybeAddStarContent(range.exclusions, node, sx_exclusion, literalExclusion)
  } yield node

  def literalStemRangeValue(x: LiteralStemRangeValue): RDFSaver[RDFNode] = x match {
    case LiteralStemRangeString(str) => ok(StringLiteral(str))
    case LiteralStemRangeWildcard() => for {
      node <- createBNode()
      _ <- addTriple(node, rdf_type, sx_Wildcard)
    } yield node
  }

  def languageStemRange(range: LanguageStemRange): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, rdf_type, sx_LanguageStemRange)
    _ <- addContent(range.stem, node, sx_stem, languageStemRangeValue)
    _ <- maybeAddStarContent(range.exclusions, node, sx_exclusion, languageExclusion)
  } yield node

  def languageStemRangeValue(x: LanguageStemRangeValue): RDFSaver[RDFNode] = x match {
    case LanguageStemRangeLang(Lang(lang)) => ok(StringLiteral(lang))
    case LanguageStemRangeWildcard() => for {
      node <- createBNode()
      _ <- addTriple(node, rdf_type, sx_Wildcard)
    } yield node
  }

  def tripleExpr(te: TripleExpr): RDFSaver[RDFNode] = te match {
    case TripleConstraint(id, inverse, negated, pred, valueExpr, min, max, semActs, annotations) => for {
      teId <- mkId(id)
      _ <- addTriple(teId, rdf_type, sx_TripleConstraint)
      _ <- maybeAddContent(inverse, teId, sx_inverse, rdfBoolean)
      _ <- maybeAddContent(negated, teId, sx_negated, rdfBoolean)
      _ <- addTriple(teId, sx_predicate, pred)
      _ <- maybeAddContent(valueExpr, teId, sx_valueExpr, shapeExpr)
      _ <- maybeAddContent(min, teId, sx_min, rdfInt)
      _ <- maybeAddContent(max, teId, sx_max, rdfMax)
      _ <- maybeAddListContent(semActs, teId, sx_semActs, semAct)
      _ <- maybeAddStarContent(annotations, teId, sx_annotation, annotation)
    } yield teId
    case EachOf(id, exprs, min, max, semActs, annotations) => for {
      node <- mkId(id)
      _ <- addTriple(node, rdf_type, sx_EachOf)
      _ <- addListContent(exprs, node, sx_expressions, tripleExpr)
      _ <- maybeAddContent(min, node, sx_min, rdfInt)
      _ <- maybeAddContent(max, node, sx_max, rdfMax)
      _ <- maybeAddListContent(semActs, node, sx_semActs, semAct)
      _ <- maybeAddStarContent(annotations, node, sx_annotation, annotation)
    } yield node
    case OneOf(id, exprs, min, max, semActs, annotations) => for {
      node <- mkId(id)
      _ <- addTriple(node, rdf_type, sx_OneOf)
      _ <- addListContent(exprs, node, sx_expressions, tripleExpr)
      _ <- maybeAddContent(min, node, sx_min, rdfInt)
      _ <- maybeAddContent(max, node, sx_max, rdfMax)
      _ <- maybeAddListContent(semActs, node, sx_semActs, semAct)
      _ <- maybeAddStarContent(annotations, node, sx_annotation, annotation)
    } yield node
    case Inclusion(lbl) => label(lbl)
  }

  def semAct(x: SemAct): RDFSaver[RDFNode] = for {
    id <- createBNode()
    _ <- addTriple(id, rdf_type, sx_SemAct)
    _ <- addTriple(id, sx_name, x.name)
    _ <- maybeAddContent(x.code, id, sx_code, rdfString)
  } yield id

  def rdfMax(x: Max): RDFSaver[RDFNode] = x match {
    case IntMax(n) => rdfInt(n)
    case Star => ok(sx_INF)
  }

  def annotation(x: Annotation): RDFSaver[RDFNode] = for {
    id <- createBNode()
    _ <- addTriple(id, rdf_type, sx_Annotation)
    _ <- addTriple(id, sx_predicate, x.predicate)
    _ <- addContent(x.obj, id, sx_object, objectValue)
  } yield id

  def objectValue(x: ObjectValue): RDFSaver[RDFNode] = x match {
    case IRIValue(iri) => ok(iri)
    case StringValue(s) => ok(StringLiteral(s))
    case DatatypeString(s, iri) => ok(DatatypeLiteral(s, iri))
    case LangString(s, lang) => ok(LangLiteral(s, lang))
  }

  def label(lbl: ShapeLabel): RDFSaver[RDFNode] = lbl match {
    case IRILabel(iri) => ok(iri)
    case BNodeLabel(bnode) => ok(bnode)
  }

  def nodeKind(nk: NodeKind): RDFSaver[RDFNode] =
    nk match {
      case IRIKind => ok(sx_iri)
      case BNodeKind => ok(sx_bnode)
      case LiteralKind => ok(sx_literal)
      case NonLiteralKind => ok(sx_nonliteral)
    }

  def mkId(id: Option[ShapeLabel]): RDFSaver[RDFNode] = id match {
    case None => createBNode
    case Some(IRILabel(iri)) => ok(iri)
    case Some(BNodeLabel(bNode)) => ok(bNode)
  }

}

object ShEx2RDF {

  def shEx2Model(s: Schema, n: Option[IRI]): Model = {
    val srdf = new ShEx2RDF {}
    srdf.toRDF(s, n, RDFAsJenaModel.empty).model
  }

}