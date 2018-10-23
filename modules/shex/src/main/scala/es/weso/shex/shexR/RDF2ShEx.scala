package es.weso.shex.shexR
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.RDFReader
import es.weso.rdf.parser.RDFParser
import es.weso.shex._
import es.weso.rdf.PREFIXES._
import es.weso.shex.shexR.PREFIXES._
import es.weso.rdf.operations.Comparisons._
import es.weso.rdf.nodes._


/* Parses RDF into SHEx.
 * The parser follows ShExR definition: https://github.com/shexSpec/shexTest/blob/master/doc/ShExR.shex
 */
trait RDF2ShEx extends RDFParser with LazyLogging {

  def getSchema(rdf: RDFReader): Either[String, Schema] = {
    val schemaNodes = rdf.triplesWithPredicateObject(rdf_type, sx_Schema).map(_.subj).toList
    val trySchemas: Either[String, List[Schema]] = parseNodes(schemaNodes, schema)(rdf)
    trySchemas match {
      case Left(e) => Left(s"Error parsing RDF as Schema: $e\nRDF: ${rdf.serialize("TURTLE")}")
      case Right(schemas) => schemas.length match {
        case 0 => Right(Schema.empty)
        case 1 => Right(schemas.head)
        case _ => {
          logger.warn(s"More than one schema obtained when parsing RDF\n${rdf.serialize("TURTLE")}")
          Right(schemas.head)
        }
      }
    }
  }

  private def schema: RDFParser[Schema] = (n, rdf) => for {
    _ <- checkType(sx_Schema)(n, rdf)
    startActions <- opt(sx_startActs, semActList1Plus)(n, rdf)
    start <- opt(sx_start, shapeExpr)(n, rdf)
    shapePairs <- starWithNodes(sx_shapes, shapeExpr)(n, rdf)
    shapes <- star(sx_shapes, shapeExpr)(n, rdf)
    // TODO: import
  } yield {
    Schema(Some(rdf.getPrefixMap()), None, startActions, start, ls2Option(shapes),None, List())
  }

  /*  def cnvShapePairs(ps: List[(RDFNode,ShapeExpr)]): Try[Map[ShapeLabel,ShapeExpr]] = {
    ps.map(cnvShapePair).sequence.map(_.toMap)
  } */

/*  private def cnvShapePair(p: (RDFNode, ShapeExpr)): Either[String, (ShapeLabel, ShapeExpr)] =
    toLabel(p._1).map(l => (l, p._2)) */

/*  private def toLabel(node: RDFNode): Either[String, ShapeLabel] = node match {
    case i: IRI => parseOk(IRILabel(i))
    case b: BNodeId => parseOk(BNodeLabel(b))
    case _ => parseFail(s"node $node must be an IRI or a BNode in order to be a ShapeLabel")
  } */

  private def shapeExpr: RDFParser[ShapeExpr] = firstOf(
    shapeOr,
    shapeAnd,
    shapeNot,
    nodeConstraint,
    shape,
    shapeExternal)

  private def shapeOr: RDFParser[ShapeOr] = (n, rdf) => for {
    _ <- checkType(sx_ShapeOr)(n, rdf)
    shapeExprs <- arc(sx_shapeExprs, shapeExprList2Plus)(n, rdf)
  } yield ShapeOr(mkId(n), shapeExprs,None,None)

  private def mkId(n: RDFNode): Option[ShapeLabel] = n match {
    case iri: IRI => Some(IRILabel(iri))
    case bnode: BNode => Some(BNodeLabel(bnode))
    case _ => None // TODO: Raise an exception?
  }

  private def shapeAnd: RDFParser[ShapeAnd] = (n, rdf) => for {
    _ <- checkType(sx_ShapeAnd)(n, rdf)
    shapeExprs <- arc(sx_shapeExprs, shapeExprList2Plus)(n, rdf)
  } yield ShapeAnd(mkId(n), shapeExprs,None,None)

  private def shapeNot: RDFParser[ShapeNot] = (n, rdf) => for {
    _ <- checkType(sx_ShapeNot)(n, rdf)
    shapeExpr <- arc(sx_shapeExpr, shapeExpr)(n, rdf)
  } yield ShapeNot(mkId(n), shapeExpr,None,None)

  private def nodeConstraint: RDFParser[NodeConstraint] = (n, rdf) => for {
    _ <- checkType(sx_NodeConstraint)(n, rdf)
    nk <- opt(sx_nodeKind, nodeKind)(n, rdf)
    datatype <- opt(sx_datatype, iri)(n, rdf)
    facets <- collect(xsFacets)(n, rdf)
    values <- opt(sx_values, valueSetValueList1Plus)(n, rdf)
  } yield NodeConstraint(mkId(n), nk, datatype, facets, values,None,None)

  private def xsFacets: List[RDFParser[XsFacet]] = List(
    length,
    minLength,
    maxLength,
    pattern,
    mininclusive,
    minexclusive,
    maxinclusive,
    maxexclusive,
    totaldigits,
    fractiondigits)

  private def length: RDFParser[Length] = (n, rdf) => for {
    value <- arc(sx_length, integer)(n, rdf)
  } yield Length(value)

  private def minLength: RDFParser[MinLength] = (n, rdf) => for {
    value <- arc(sx_minlength, integer)(n, rdf)
  } yield MinLength(value)

  private def maxLength: RDFParser[MaxLength] = (n, rdf) => for {
    value <- arc(sx_maxlength, integer)(n, rdf)
  } yield MaxLength(value)

  private def pattern: RDFParser[Pattern] = (n, rdf) => for {
    value <- arc(sx_pattern, string)(n, rdf)
    flags <- opt(sx_flags, string)(n, rdf)
  } yield Pattern(value, flags)

  private def mininclusive: RDFParser[MinInclusive] = (n, rdf) => for {
    value <- arc(sx_mininclusive, numericLiteral)(n, rdf)
  } yield MinInclusive(value)

  private def minexclusive: RDFParser[MinExclusive] = (n, rdf) => for {
    value <- arc(sx_minexclusive, numericLiteral)(n, rdf)
  } yield MinExclusive(value)

  private def maxinclusive: RDFParser[MaxInclusive] = (n, rdf) => for {
    value <- arc(sx_maxinclusive, numericLiteral)(n, rdf)
  } yield MaxInclusive(value)

  private def maxexclusive: RDFParser[MaxExclusive] = (n, rdf) => for {
    value <- arc(sx_maxexclusive, numericLiteral)(n, rdf)
  } yield MaxExclusive(value)

  private def numericLiteral: RDFParser[NumericLiteral] = (n, rdf) => n match {
    case IntegerLiteral(n,repr) => parseOk(NumericInt(n, repr))
    case DoubleLiteral(d,repr) => parseOk(NumericDouble(d, repr))
    case DecimalLiteral(d,repr) => parseOk(NumericDecimal(d, repr))
    case _ => parseFail(s"Expected numeric literal but found $n")
  }

  private def fractiondigits: RDFParser[FractionDigits] = (n, rdf) => for {
    value <- arc(sx_fractiondigits, integer)(n, rdf)
  } yield FractionDigits(value)

  private def totaldigits: RDFParser[TotalDigits] = (n, rdf) => for {
    value <- arc(sx_totaldigits, integer)(n, rdf)
  } yield TotalDigits(value)

  private def nodeKind: RDFParser[NodeKind] = (n, rdf) => n match {
    case `sx_iri` => parseOk(IRIKind)
    case `sx_bnode` => parseOk(BNodeKind)
    case `sx_literal` => parseOk(LiteralKind)
    case `sx_nonliteral` => parseOk(NonLiteralKind)
    case _ => parseFail(s"Expected nodekind, found: $n")
  }

  private def shape: RDFParser[Shape] = (n, rdf) => for {
    _ <- checkType(sx_Shape)(n, rdf)
    closed <- opt(sx_closed, boolean)(n, rdf)
    extras <- star(sx_extra, iri)(n, rdf)
    expression <- opt(sx_expression, tripleExpression)(n, rdf)
    semActs <- opt(sx_semActs, semActList1Plus)(n, rdf)
    annotations <- star(sx_annotation, annotationParser)(n, rdf)
  } yield Shape(mkId(n), None, closed, ls2Option(extras), expression, None, ls2Option(annotations), semActs)

  private def shapeExternal: RDFParser[ShapeExternal] = (n, rdf) => for {
    _ <- checkType(sx_ShapeExternal)(n, rdf)
  } yield ShapeExternal(mkId(n),None,None)

  private def semAct: RDFParser[SemAct] = (n, rdf) => for {
    _ <- checkType(sx_SemAct)(n, rdf)
    name <- iriFromPredicate(sx_name)(n, rdf)
    code <- optional(stringFromPredicate(sx_code))(n, rdf)
  } yield SemAct(name, code)

  private def tripleExpression: RDFParser[TripleExpr] =
    firstOf(tripleConstraint, oneOf, eachOf)

  private def tripleConstraint: RDFParser[TripleConstraint] = (n, rdf) => for {
    _ <- checkType(sx_TripleConstraint)(n, rdf)
    optInverse <- opt(sx_inverse, boolean)(n, rdf)
    optNegated <- opt(sx_negated, boolean)(n, rdf)
    optMin <- opt(sx_min, integer)(n, rdf)
    optMax <- opt(sx_max, max)(n, rdf)
    predicate <- arc(sx_predicate, iri)(n, rdf)
    valueExpr <- opt(sx_valueExpr, shapeExpr)(n, rdf)
    semActs <- opt(sx_semActs, semActList1Plus)(n, rdf)
    annotations <- star(sx_annotation, annotationParser)(n, rdf)
  } yield // TODO: Variable decl
    TripleConstraint(
    mkId(n),
    optInverse, optNegated, predicate, valueExpr, optMin, optMax, None, semActs,
    ls2Option(annotations))

  private def valueSetValue: RDFParser[ValueSetValue] = firstOf(
    objectValue,
    iriStem,
    iriStemRange,
    literalStem,
    literalStemRange,
    languageStem,
    languageStemRange
  )

  private def iriStem: RDFParser[IRIStem] = (n, rdf) => for {
    _ <- checkType(sx_IriStem)(n, rdf)
    str <- arc(sx_stem, anyUri)(n, rdf)
  } yield IRIStem(IRI(str))

  private def literalStem: RDFParser[LiteralStem] = (n, rdf) => for {
    _ <- checkType(sx_LiteralStem)(n, rdf)
    str <- arc(sx_stem, string)(n, rdf)
  } yield LiteralStem(str)

  private def languageStem: RDFParser[LanguageStem] = (n, rdf) => for {
    _ <- checkType(sx_LanguageStem)(n, rdf)
    langTag <- arc(sx_stem, string)(n, rdf)
  } yield LanguageStem(Lang(langTag))

  private def iriStemRange: RDFParser[IRIStemRange] = (n, rdf) => for {
    _ <- checkType(sx_IriStemRange)(n, rdf)
    sv <- arc(sx_stem, iriStemRangeValue)(n, rdf)
    exclusions <- star(sx_exclusion, iriExclusion)(n, rdf)
  } yield IRIStemRange(sv, ls2Option(exclusions))

  private def iriStemRangeValue: RDFParser[IRIStemRangeValue] = firstOf(
    iriStemValueIRI,
    iriStemWildCard
  )

  private def literalStemRange: RDFParser[LiteralStemRange] = (n, rdf) => for {
    _ <- checkType(sx_LiteralStemRange)(n, rdf)
    sv <- arc(sx_stem, literalStemRangeValue)(n, rdf)
    exclusions <- star(sx_exclusion, literalExclusion)(n, rdf)
  } yield LiteralStemRange(sv, ls2Option(exclusions))

  private def literalStemRangeValue: RDFParser[LiteralStemRangeValue] = firstOf(
    literalStemRangeString,
    literalStemWildCard,
  )

  private def literalStemRangeString: RDFParser[LiteralStemRangeString] = (n,rdf) => n match {
    case StringLiteral(str) => parseOk(LiteralStemRangeString(str))
    case _ => parseFail(s"LiteralStemRangeString: Expected string for $n")
  }

  private def languageStemRange: RDFParser[LanguageStemRange] = (n, rdf) => for {
    _ <- checkType(sx_LanguageStemRange)(n, rdf)
    sv <- arc(sx_stem, languageStemRangeValue)(n, rdf)
    exclusions <- star(sx_exclusion, languageExclusion)(n, rdf)
  } yield LanguageStemRange(sv, ls2Option(exclusions))

  private def languageStemRangeValue: RDFParser[LanguageStemRangeValue] = firstOf(
    languageStemRangeLang,
    languageStemWildCard,
  )

  private def languageStemRangeLang: RDFParser[LanguageStemRangeLang] = (n,rdf) => n match {
    case StringLiteral(str) => parseOk(LanguageStemRangeLang(Lang(str)))
    case _ => parseFail(s"LanguageStemRangeLang: Expected string for $n")
  }

  private def iriExclusion: RDFParser[IRIExclusion] = firstOf(
    iriRefExclusion,
    iriStemExclusion
  )

  private def languageExclusion: RDFParser[LanguageExclusion] = firstOf(
    languageTagExclusion,languageStemExclusion
  )

  private def literalExclusion: RDFParser[LiteralExclusion] = firstOf(
    literalStringExclusion,literalStemExclusion
  )

  private def iriRefExclusion: RDFParser[IRIRefExclusion] = (n, rdf) => n match {
    case iri: IRI => parseOk(IRIRefExclusion(iri))
    case _ => parseFail(s"iriRefExclusion: expected an IRI for $n")
  }

  private def iriStemExclusion: RDFParser[IRIStemExclusion] = (n, rdf) => for {
    iriStem <- iriStem(n,rdf)
  } yield IRIStemExclusion(iriStem)

  private def literalStringExclusion: RDFParser[LiteralStringExclusion] = (n, rdf) => n match {
    case StringLiteral(str) => parseOk(LiteralStringExclusion(str))
    case _ => parseFail(s"literalStringExclusion: expected a StringLiteral for $n")
  }

  private def literalStemExclusion: RDFParser[LiteralStemExclusion] = (n, rdf) => for {
    literalStem <- literalStem(n,rdf)
  } yield LiteralStemExclusion(literalStem)


  private def languageTagExclusion: RDFParser[LanguageTagExclusion] = {
    throw new Exception(s"Not implemented languageTagExclusion yet")
  }
  private def languageStemExclusion: RDFParser[LanguageStemExclusion] = {
    throw new Exception(s"Not implemented languageStemExclusion yet")
  }

  private def iriStemValueIRI: RDFParser[IRIStemRangeValue] = (n, rdf) => for {
    str <- anyUri(n, rdf)
  } yield IRIStemValueIRI(IRI(str))

  private def iriStemWildCard: RDFParser[IRIStemRangeValue] = (n, rdf) => for {
    _ <- checkType(sx_Wildcard)(n, rdf)
  } yield IRIStemWildcard()

  private def literalStemWildCard: RDFParser[LiteralStemRangeValue] = (n, rdf) => for {
    _ <- checkType(sx_Wildcard)(n, rdf)
  } yield LiteralStemRangeWildcard()

  private def languageStemWildCard: RDFParser[LanguageStemRangeValue] = (n, rdf) => for {
    _ <- checkType(sx_Wildcard)(n, rdf)
  } yield LanguageStemRangeWildcard()

  /*  private def objectValueStem: RDFParser[ValueSetValue] =
      firstOf(objectValue, stem) */

  private def anyUri: RDFParser[String] = (n, rdf) => n match {
    case DatatypeLiteral(str, iri) if iri == xsd_anyUri => parseOk(str)
    case _ => parseFail(s"Expected typed literal with datatype xsd:anyUri. Obtained: $n")
  }

  private def oneOf: RDFParser[OneOf] = (n, rdf) => for {
    _ <- checkType(sx_OneOf)(n, rdf)
    optMin <- opt(sx_min, integer)(n, rdf)
    optMax <- opt(sx_max, max)(n, rdf)
    expressions <- arc(sx_expressions, tripleExpressionList2Plus)(n, rdf)
    semActs <- opt(sx_semActs, semActList1Plus)(n, rdf)
    annotations <- star(sx_annotation, annotationParser)(n, rdf)
  } yield OneOf(mkId(n), expressions, optMin, optMax, semActs, ls2Option(annotations))

  private def eachOf: RDFParser[EachOf] = (n, rdf) => for {
    _ <- checkType(sx_EachOf)(n, rdf)
    optMin <- opt(sx_min, integer)(n, rdf)
    optMax <- opt(sx_max, max)(n, rdf)
    expressions <- arc(sx_expressions, tripleExpressionList2Plus)(n, rdf)
    semActs <- opt(sx_semActs, semActList1Plus)(n, rdf)
    annotations <- star(sx_annotation, annotationParser)(n, rdf)
  } yield EachOf(mkId(n), expressions, optMin, optMax, semActs, ls2Option(annotations))

  private def ls2Option[A](ls: List[A]): Option[List[A]] =
    if (ls.isEmpty) None else Some(ls)

  private def annotationParser: RDFParser[Annotation] = (n, rdf) => for {
    _ <- checkType(sx_Annotation)(n, rdf)
    pred <- arc(sx_predicate, iri)(n, rdf)
    obj <- arc(sx_object, objectValue)(n, rdf)
  } yield Annotation(pred, obj)

  private def objectValue: RDFParser[ObjectValue] = (n, rdf) => n match {
    case iri: IRI => parseOk(IRIValue(iri))
    case StringLiteral(str) => parseOk(StringValue(str))
    case DatatypeLiteral(str, iri) => parseOk(DatatypeString(str, iri))
    case LangLiteral(lex, lan) => parseOk(LangString(lex, lan))
    case _ => parseFail(s"Unexpected object value: $n must be an IRI or a Literal")
  }

  private def max: RDFParser[Max] = (n, rdf) => n match {
    case IntegerLiteral(n,_) => parseOk(IntMax(n))
    case StringLiteral("*") => parseOk(Star)
    case _ => parseFail(s"Unexpected node parsing max cardinality: $n")
  }

  private def tripleExpressionList2Plus: RDFParser[List[TripleExpr]] =
    list2Plus(tripleExpression)

  private def semActList1Plus: RDFParser[List[SemAct]] =
    list1Plus(semAct)

  private def shapeExprList2Plus: RDFParser[List[ShapeExpr]] =
    list2Plus(shapeExpr)

/*  private def shapeExprList1Plus: RDFParser[List[ShapeExpr]] =
    list1Plus(shapeExpr) */

  private def valueSetValueList1Plus: RDFParser[List[ValueSetValue]] =
    list1Plus(valueSetValue)

}

object RDF2ShEx extends RDF2ShEx {

  def rdf2Schema(rdf: RDFReader): Either[String, Schema] =
    getSchema(rdf)

}
