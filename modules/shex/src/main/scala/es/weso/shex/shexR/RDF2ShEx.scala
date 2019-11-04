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

  val initialNode = BNode("internalNode")
  def getSchema(rdf: RDFReader): Either[String, Schema] = for {
    schemaNodes <- rdf.triplesWithPredicateObject(`rdf:type`, sx_Schema) // .map(_.subj).toList
    cfg = Config(initialNode,rdf)
    schemas <- 
      parseNodes(schemaNodes.toList.map(_.subj), schema).value.run(cfg)
    r <- schemas.length match {
        case 0 => Right(Schema.empty)
        case 1 => Right(schemas.head)
        case _ => {
          logger.warn(s"More than one schema obtained when parsing RDF\n${rdf.serialize("TURTLE")}")
          Right(schemas.head)
        }
      }
  } yield r

  private def schema: RDFParser[Schema] = for {
    rdf <- getRDF
    _ <- checkType(sx_Schema)
    startActions <- opt(sx_startActs, semActList1Plus)
    start <- opt(sx_start, shapeExpr)
    shapePairs <- starWithNodes(sx_shapes, shapeExpr)
    shapes <- star(sx_shapes, shapeExpr)
    // TODO: import
  } yield {
    Schema(IRI(""),Some(rdf.getPrefixMap()), None, startActions, start, ls2Option(shapes),None, List())
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

  private def shapeOr: RDFParser[ShapeExpr] = for {
    n <- getNode
    _ <- checkType(sx_ShapeOr)
    shapeExprs <- arc(sx_shapeExprs, shapeExprList2Plus)
  } yield ShapeOr(mkId(n), shapeExprs,None,None)

  private def mkId(n: RDFNode): Option[ShapeLabel] = n match {
    case iri: IRI => Some(IRILabel(iri))
    case bnode: BNode => Some(BNodeLabel(bnode))
    case _ => None // TODO: Raise an exception?
  }

  private def shapeAnd: RDFParser[ShapeExpr] = for {
    n <- getNode
    _ <- checkType(sx_ShapeAnd)
    shapeExprs <- arc(sx_shapeExprs, shapeExprList2Plus)
  } yield ShapeAnd(mkId(n), shapeExprs,None,None)

  private def shapeNot: RDFParser[ShapeExpr] = for {
    n <- getNode
    _ <- checkType(sx_ShapeNot)
    shapeExpr <- arc(sx_shapeExpr, shapeExpr)
  } yield ShapeNot(mkId(n), shapeExpr,None,None)

  private def nodeConstraint: RDFParser[ShapeExpr] = for {
    n <- getNode
    _ <- checkType(sx_NodeConstraint)
    nk <- opt(sx_nodeKind, nodeKind)
    datatype <- opt(sx_datatype, iri)
    facets <- collect(xsFacets)
    values <- opt(sx_values, valueSetValueList1Plus)
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

  private def length: RDFParser[XsFacet] = for {
    value <- arc(sx_length, integer)
  } yield Length(value)

  private def minLength: RDFParser[XsFacet] = for {
    value <- arc(sx_minlength, integer)
  } yield MinLength(value)

  private def maxLength: RDFParser[XsFacet] = for {
    value <- arc(sx_maxlength, integer)
  } yield MaxLength(value)

  private def pattern: RDFParser[XsFacet] = for {
    value <- arc(sx_pattern, string)
    flags <- opt(sx_flags, string)
  } yield Pattern(value, flags)

  private def mininclusive: RDFParser[XsFacet] = for {
    value <- arc(sx_mininclusive, numericLiteral)
  } yield MinInclusive(value)

  private def minexclusive: RDFParser[XsFacet] = for {
    value <- arc(sx_minexclusive, numericLiteral)
  } yield MinExclusive(value)

  private def maxinclusive: RDFParser[XsFacet] = for {
    value <- arc(sx_maxinclusive, numericLiteral)
  } yield MaxInclusive(value)

  private def maxexclusive: RDFParser[XsFacet] = for {
    value <- arc(sx_maxexclusive, numericLiteral)
  } yield MaxExclusive(value)

  private def numericLiteral: RDFParser[NumericLiteral] = for {
    n <- getNode
    v <- n match {
     case IntegerLiteral(n,repr) => parseOk(NumericInt(n, repr))
     case DoubleLiteral(d,repr) => parseOk(NumericDouble(d, repr))
     case DecimalLiteral(d,repr) => parseOk(NumericDecimal(d, repr))
     case _ => parseFail(s"Expected numeric literal but found $n")
    } 
  } yield v

  private def fractiondigits: RDFParser[XsFacet] = for {
    value <- arc(sx_fractiondigits, integer)
  } yield FractionDigits(value)

  private def totaldigits: RDFParser[XsFacet] = for {
    value <- arc(sx_totaldigits, integer)
  } yield TotalDigits(value)

  private def nodeKind: RDFParser[NodeKind] = for {
    n <- getNode
    v <- n match {
     case `sx_iri` => parseOk(IRIKind)
     case `sx_bnode` => parseOk(BNodeKind)
     case `sx_literal` => parseOk(LiteralKind)
     case `sx_nonliteral` => parseOk(NonLiteralKind)
     case _ => parseFail(s"Expected nodekind, found: $n")
    }
  } yield v 

  private def shape: RDFParser[ShapeExpr] = for {
    n <- getNode
    _ <- checkType(sx_Shape)
    closed <- opt(sx_closed, boolean)
    extras <- star(sx_extra, iri)
    expression <- opt(sx_expression, tripleExpression)
    semActs <- opt(sx_semActs, semActList1Plus)
    annotations <- star(sx_annotation, annotationParser)
  } yield Shape(mkId(n), None, closed, ls2Option(extras), expression, None, ls2Option(annotations), semActs)

  private def shapeExternal: RDFParser[ShapeExpr] = for {
    n <- getNode
    _ <- checkType(sx_ShapeExternal)
  } yield ShapeExternal(mkId(n),None,None)

  private def semAct: RDFParser[SemAct] = for {
    _ <- checkType(sx_SemAct)
    name <- iriFromPredicate(sx_name)
    code <- optional(stringFromPredicate(sx_code))
  } yield SemAct(name, code)

  private def tripleExpression: RDFParser[TripleExpr] =
    firstOf(tripleConstraint, oneOf, eachOf)

  private def tripleConstraint: RDFParser[TripleExpr] = for {
    n <- getNode
    _ <- checkType(sx_TripleConstraint)
    optInverse <- opt(sx_inverse, boolean)
    optNegated <- opt(sx_negated, boolean)
    optMin <- opt(sx_min, integer)
    optMax <- opt(sx_max, max)
    predicate <- arc(sx_predicate, iri)
    valueExpr <- opt(sx_valueExpr, shapeExpr)
    semActs <- opt(sx_semActs, semActList1Plus)
    annotations <- star(sx_annotation, annotationParser)
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

  private def iriStem: RDFParser[ValueSetValue] = for {
    _ <- checkType(sx_IriStem)
    str <- arc(sx_stem, anyUri)
  } yield IRIStem(IRI(str))

  private def literalStem: RDFParser[ValueSetValue] = for {
    _ <- checkType(sx_LiteralStem)
    str <- arc(sx_stem, string)
  } yield LiteralStem(str)

  private def languageStem: RDFParser[ValueSetValue] = for {
    _ <- checkType(sx_LanguageStem)
    langTag <- arc(sx_stem, string)
  } yield LanguageStem(Lang(langTag))

  private def iriStemRange: RDFParser[ValueSetValue] = for {
    _ <- checkType(sx_IriStemRange)
    sv <- arc(sx_stem, iriStemRangeValue)
    exclusions <- star(sx_exclusion, iriExclusion)
  } yield IRIStemRange(sv, ls2Option(exclusions))

  private def iriStemRangeValue: RDFParser[IRIStemRangeValue] = 
  firstOf(
    iriStemValueIRI,
    iriStemWildCard
  )

  private def literalStemRange: RDFParser[ValueSetValue] = for {
    _ <- checkType(sx_LiteralStemRange)
    sv <- arc(sx_stem, literalStemRangeValue)
    exclusions <- star(sx_exclusion, literalExclusion)
  } yield LiteralStemRange(sv, ls2Option(exclusions))

  private def literalStemRangeValue: RDFParser[LiteralStemRangeValue] = 
   firstOf(
    literalStemRangeString,
    literalStemRangeWildCard,
   )

  private def literalStemRangeString: RDFParser[LiteralStemRangeValue] = for {
    n <- getNode 
    v <- n match {
     case StringLiteral(str) => parseOk(LiteralStemRangeString(str))
     case _ => parseFail(s"LiteralStemRangeString: Expected string for $n")
   }
  } yield v

  private def literalStemRangeWildCard: RDFParser[LiteralStemRangeValue] = for {
    _ <- checkType(sx_Wildcard)
  } yield LiteralStemRangeWildcard()

  private def languageStemRange: RDFParser[ValueSetValue] = for {
    _ <- checkType(sx_LanguageStemRange)
    sv <- arc(sx_stem, languageStemRangeValue)
    exclusions <- star(sx_exclusion, languageExclusion)
  } yield LanguageStemRange(sv, ls2Option(exclusions))

  private def languageStemRangeValue: RDFParser[LanguageStemRangeValue] = 
   firstOf(
    languageStemRangeLang,
    languageStemWildCard,
   )

  private def languageStemRangeLang: RDFParser[LanguageStemRangeValue] = for {
    n <- getNode
    v <- n match {
     case StringLiteral(str) => parseOk(LanguageStemRangeLang(Lang(str)))
     case _ => parseFail(s"LanguageStemRangeLang: Expected string for $n")
    }
   } yield v

  private def iriExclusion: RDFParser[IRIExclusion] = firstOf(
    iriRefExclusion,
    iriStemExclusion
  )

  private def languageExclusion: RDFParser[LanguageExclusion] = 
   firstOf(
    languageTagExclusion,languageStemExclusion
   )

  private def literalExclusion: RDFParser[LiteralExclusion] = 
   firstOf(
    literalStringExclusion,literalStemExclusion
   )

  private def iriRefExclusion: RDFParser[IRIExclusion] = for { 
    n <- getNode 
    v <- n match {
    case iri: IRI => parseOk(IRIRefExclusion(iri))
    case _ => parseFail(s"iriRefExclusion: expected an IRI for $n")
   }
  } yield v

  private def iriStemExclusion: RDFParser[IRIExclusion] = for {
    vs <- iriStem
    iriStem <- vs match {
      case i: IRIStem => ok(i)
      case _ => parseFail(s"Expected iriStem")
    }
  } yield IRIStemExclusion(iriStem)

  private def literalStringExclusion: RDFParser[LiteralExclusion] =    for {
    n <- getNode 
    v <- n match {
     case StringLiteral(str) => parseOk(LiteralStringExclusion(str))
     case _ => parseFail(s"literalStringExclusion: expected a StringLiteral for $n")
    }
  } yield v 

  private def literalStemExclusion: RDFParser[LiteralExclusion] = for {
    vsv <- literalStem
    ls <- vsv match {
      case l: LiteralStem => ok(l)
      case _ => parseFail(s"Expected $vsv to be a LiteralStem")
    }
  } yield LiteralStemExclusion(ls)

  private def languageTagExclusion: RDFParser[LanguageExclusion] = {
    throw new Exception(s"Not implemented languageTagExclusion yet")
  }

  private def languageStemExclusion: RDFParser[LanguageExclusion] = {
    throw new Exception(s"Not implemented languageStemExclusion yet")
  }

  private def iriStemValueIRI: RDFParser[IRIStemRangeValue] = for {
    str <- anyUri
  } yield IRIStemValueIRI(IRI(str))

  private def iriStemWildCard: RDFParser[IRIStemRangeValue] = for {
    _ <- checkType(sx_Wildcard)
  } yield IRIStemWildcard()

  private def languageStemWildCard: RDFParser[LanguageStemRangeValue] = 
  for {
    _ <- checkType(sx_Wildcard)
  } yield LanguageStemRangeWildcard()

  private def anyUri: RDFParser[String] = for {
    n <- getNode
    v <- n match {
     case DatatypeLiteral(str, iri) if iri == `xsd:anyUri` => parseOk(str)
     case _ => parseFail(s"Expected typed literal with datatype xsd:anyUri. Obtained: $n")
   }
  } yield v

  private def oneOf: RDFParser[TripleExpr] = for {
    n <- getNode
    _ <- checkType(sx_OneOf)
    optMin <- opt(sx_min, integer)
    optMax <- opt(sx_max, max)
    expressions <- arc(sx_expressions, tripleExpressionList2Plus)
    semActs <- opt(sx_semActs, semActList1Plus)
    annotations <- star(sx_annotation, annotationParser)
  } yield OneOf(mkId(n), expressions, optMin, optMax, semActs, ls2Option(annotations))

  private def eachOf: RDFParser[TripleExpr] = for {
    n <- getNode
    _ <- checkType(sx_EachOf)
    optMin <- opt(sx_min, integer)
    optMax <- opt(sx_max, max)
    expressions <- arc(sx_expressions, tripleExpressionList2Plus)
    semActs <- opt(sx_semActs, semActList1Plus)
    annotations <- star(sx_annotation, annotationParser)
  } yield EachOf(mkId(n), expressions, optMin, optMax, semActs, ls2Option(annotations))

  private def ls2Option[A](ls: List[A]): Option[List[A]] =
    if (ls.isEmpty) None else Some(ls)

  private def annotationParser: RDFParser[Annotation] = for {
    _ <- checkType(sx_Annotation)
    pred <- arc(sx_predicate, iri)
    vsv <- arc(sx_object, objectValue)
    obj <- vsv match {
      case o: ObjectValue => ok(o)
      case _ => parseFail(s"Expected $vsv to be an ObjectValue")
    }
  } yield Annotation(pred, obj)

  private def objectValue: RDFParser[ValueSetValue] = for {
    n <- getNode
    v <- n match {
    case iri: IRI => parseOk(IRIValue(iri))
    case StringLiteral(str) => parseOk(StringValue(str))
    case DatatypeLiteral(str, iri) => parseOk(DatatypeString(str, iri))
    case LangLiteral(lex, lan) => parseOk(LangString(lex, lan))
    case _ => parseFail(s"Unexpected object value: $n must be an IRI or a Literal")
   }
  } yield v

  private def max: RDFParser[Max] = for {
    n <- getNode
    v <- n match {
    case IntegerLiteral(n,_) => parseOk(IntMax(n))
    case StringLiteral("*") => parseOk(Star)
    case _ => parseFail(s"Unexpected node parsing max cardinality: $n")
   }
  } yield v

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
