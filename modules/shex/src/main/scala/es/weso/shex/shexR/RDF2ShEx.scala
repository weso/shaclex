package es.weso.shex.shexR
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.RDFReader
import es.weso.rdf.parser.RDFParser
import es.weso.shex._
import es.weso.rdf.PREFIXES._
import es.weso.shex.shexR.PREFIXES._
import cats._
import cats.implicits._
import es.weso.rdf.nodes._

import scala.util.{Failure, Success, Try}

/* Parses RDF into SHEx.
 * The parser follows ShExR definition: https://github.com/shexSpec/shexTest/blob/master/doc/ShExR.shex
 */
trait RDF2ShEx extends RDFParser with LazyLogging {

  def getSchema(rdf: RDFReader): Either[String,Schema] = {
    val schemaNodes = rdf.triplesWithPredicateObject(rdf_type, sx_Schema).map(_.subj).toList
    val trySchemas: Try[List[Schema]] = schemaNodes.map(schema(_,rdf)).sequence
    trySchemas match {
      case Failure(e) => Left(s"Error parsing RDF as Schema: $e\nRDF: ${rdf.serialize("TURTLE")}")
      case Success(schemas) => schemas.length match {
        case 0 => Left(s"Empty schema parsing RDF\nRDF: ${rdf.serialize("TURTLE")}")
        case 1 => Right(schemas.head)
        case _ => {
          logger.warn(s"More than one schema obtained when parsing RDF\n${rdf.serialize("TURTLE")}")
          Right(schemas.head)
        }
      }
    }
  }

  def schema: RDFParser[Schema] = (n,rdf) => for {
    _ <- checkType(sx_Schema)(n,rdf)
    startActions <- opt(sx_startActs, semActList1Plus)(n,rdf)
    start <- opt(sx_start, shapeExpr)(n,rdf)
    shapePairs <- starWithNodes(sx_shapes, shapeExpr)(n,rdf)
    shapes <- star(sx_shapes, shapeExpr)(n,rdf)
  } yield {
    Schema(Some(rdf.getPrefixMap()),None,startActions,start, ls2Option(shapes))
  }

/*  def cnvShapePairs(ps: List[(RDFNode,ShapeExpr)]): Try[Map[ShapeLabel,ShapeExpr]] = {
    ps.map(cnvShapePair).sequence.map(_.toMap)
  } */

  def cnvShapePair(p: (RDFNode,ShapeExpr)): Try[(ShapeLabel,ShapeExpr)] =
    toLabel(p._1).map(l => (l,p._2))

  def toLabel(node: RDFNode): Try[ShapeLabel] = node match {
    case i: IRI => Success(IRILabel(i))
    case b: BNodeId => Success(BNodeLabel(b))
    case _ => parseFail(s"node $node must be an IRI or a BNode in order to be a ShapeLabel")
  }

  def shapeExpr: RDFParser[ShapeExpr] = firstOf(
    shapeOr,
    shapeAnd,
    shapeNot,
    nodeConstraint,
    shape,
    shapeExternal
  )

  def shapeOr: RDFParser[ShapeOr] = (n,rdf) => for {
    _ <- checkType(sx_ShapeOr)(n,rdf)
    shapeExprs <- arc(sx_shapeExprs, shapeExprList2Plus)(n,rdf)
  } yield ShapeOr(mkId(n), shapeExprs)

  def mkId(n: RDFNode): Option[ShapeLabel] = n match {
    case iri: IRI => Some(IRILabel(iri))
    case bnode: BNodeId => Some(BNodeLabel(bnode))
    case _ => None  // TODO: Raise an exception?
  }

  def shapeAnd: RDFParser[ShapeAnd] = (n,rdf) => for {
    _ <- checkType(sx_ShapeAnd)(n,rdf)
    shapeExprs <- arc(sx_shapeExprs, shapeExprList2Plus)(n,rdf)
  } yield ShapeAnd(mkId(n),shapeExprs)

  def shapeNot: RDFParser[ShapeNot] = (n,rdf) => for {
    _ <- checkType(sx_ShapeNot)(n,rdf)
    shapeExpr <- arc(sx_shapeExpr, shapeExpr)(n,rdf)
  } yield ShapeNot(mkId(n),shapeExpr)

  def nodeConstraint: RDFParser[NodeConstraint] = (n,rdf) => for {
    _ <- checkType(sx_NodeConstraint)(n,rdf)
    nk <- opt(sx_nodeKind,nodeKind)(n,rdf)
    datatype <- opt(sx_datatype, iri)(n,rdf)
    facets <- collect(xsFacets)(n,rdf)
    values <- opt(sx_values, valueSetValueList1Plus)(n,rdf)
  } yield NodeConstraint(mkId(n),nk,datatype,facets,values)

  def xsFacets: List[RDFParser[XsFacet]] = List(
    length,
    minLength,
    maxLength,
    pattern,
    mininclusive,
    minexclusive,
    maxinclusive,
    maxexclusive,
    totaldigits,
    fractiondigits
  )

  def length: RDFParser[Length] = (n,rdf) => for {
    value <- arc(sx_length,integer)(n,rdf)
  } yield Length(value)

  def minLength: RDFParser[MinLength] = (n,rdf) => for {
    value <- arc(sx_minlength,integer)(n,rdf)
  } yield MinLength(value)

  def maxLength: RDFParser[MaxLength] = (n,rdf) => for {
    value <- arc(sx_maxlength,integer)(n,rdf)
  } yield MaxLength(value)

  def pattern: RDFParser[Pattern] = (n,rdf) => for {
    value <- arc(sx_pattern,string)(n,rdf)
  } yield Pattern(value)

  def mininclusive: RDFParser[MinInclusive] = (n,rdf) => for {
    value <- arc(sx_mininclusive,numericLiteral)(n,rdf)
  } yield MinInclusive(value)

  def minexclusive: RDFParser[MinExclusive] = (n,rdf) => for {
    value <- arc(sx_minexclusive,numericLiteral)(n,rdf)
  } yield MinExclusive(value)

  def maxinclusive: RDFParser[MaxInclusive] = (n,rdf) => for {
    value <- arc(sx_maxinclusive,numericLiteral)(n,rdf)
  } yield MaxInclusive(value)

  def maxexclusive: RDFParser[MaxExclusive] = (n,rdf) => for {
    value <- arc(sx_maxexclusive,numericLiteral)(n,rdf)
  } yield MaxExclusive(value)

  def numericLiteral: RDFParser[NumericLiteral] = (n,rdf) => n match {
    case IntegerLiteral(n) => Success(NumericInt(n))
    case DoubleLiteral(d) => Success(NumericDouble(d))
    case DecimalLiteral(d) => Success(NumericDecimal(d))
    case _ => parseFail(s"Expected numeric literal but found $n")
  }

  def fractiondigits: RDFParser[FractionDigits] = (n,rdf) => for {
    value <- arc(sx_fractiondigits,integer)(n,rdf)
  } yield FractionDigits(value)

  def totaldigits: RDFParser[TotalDigits] = (n,rdf) => for {
    value <- arc(sx_totaldigits,integer)(n,rdf)
  } yield TotalDigits(value)

  def nodeKind: RDFParser[NodeKind] = (n,rdf) => n match {
    case `sx_iri` => Success(IRIKind)
    case `sx_bnode` => Success(BNodeKind)
    case `sx_literal` => Success(LiteralKind)
    case `sx_nonliteral` => Success(NonLiteralKind)
    case _ => parseFail(s"Expected nodekind, found: $n")
  }

  def shape: RDFParser[Shape] = (n,rdf) => for {
    _ <- checkType(sx_Shape)(n,rdf)
    closed <- opt(sx_closed, boolean)(n,rdf)
    extras <- star(sx_extra, iri)(n,rdf)
    expression <- opt(sx_expression, tripleExpression)(n,rdf)
    semActs <- opt(sx_semActs, semActList1Plus)(n,rdf)
  } yield Shape(mkId(n),None, closed, ls2Option(extras), expression, None, semActs)

  def shapeExternal: RDFParser[ShapeExternal] = (n,rdf) => for {
    _ <- checkType(sx_ShapeExternal)(n,rdf)
  } yield ShapeExternal(mkId(n))

  def semAct: RDFParser[SemAct] = (n,rdf) => for {
    _ <- checkType(sx_SemAct)(n,rdf)
    name <- iriFromPredicate(sx_name)(n,rdf)
    code <- optional(stringFromPredicate(sx_code))(n,rdf)
  } yield SemAct(name,code)

  def tripleExpression: RDFParser[TripleExpr] =
    firstOf(tripleConstraint, oneOf, eachOf)

  def tripleConstraint: RDFParser[TripleConstraint] = (n,rdf) => for {
    _ <- checkType(sx_TripleConstraint)(n,rdf)
    optInverse <- opt(sx_inverse,boolean)(n,rdf)
    optNegated <- opt(sx_negated,boolean)(n,rdf)
    optMin <- opt(sx_min,integer)(n,rdf)
    optMax <- opt(sx_max,max)(n,rdf)
    predicate <- arc(sx_predicate,iri)(n,rdf)
    valueExpr <- opt(sx_valueExpr,shapeExpr)(n,rdf)
    semActs <- opt(sx_semActs, semActList1Plus)(n,rdf)
    annotations <- star(sx_annotation,annotationParser)(n,rdf)
  } yield TripleConstraint(mkId(n),
     optInverse,optNegated,predicate,valueExpr,optMin,optMax,semActs,
     ls2Option(annotations
     )
  )

  def valueSetValue: RDFParser[ValueSetValue] = firstOf(
    objectValue,
    stem,
    stemRange
  )

  def stem: RDFParser[Stem] = (n,rdf) => for {
    _ <- checkType(sx_Stem)(n,rdf)
    str <- arc(sx_stem,anyUri)(n,rdf)
  } yield Stem(IRI(str))

  def stemRange: RDFParser[StemRange] = (n,rdf) => for {
    _ <- checkType(sx_StemRange)(n,rdf)
    sv <- arc(sx_stem,stemValue)(n,rdf)
    exclusions <- star(sx_exclusion, objectValueStem)(n,rdf)
  } yield StemRange(sv, ls2Option(exclusions))

  def stemValue: RDFParser[StemValue] = firstOf(
    anyUriStem,
    wildCard
  )

  def anyUriStem: RDFParser[StemValue] = (n,rdf) => for {
    str <- anyUri(n,rdf)
  } yield IRIStem(IRI(str))

  def wildCard: RDFParser[StemValue] = (n,rdf) => for {
    _ <- checkType(sx_Wildcard)(n,rdf)
  } yield Wildcard()

  def objectValueStem: RDFParser[ValueSetValue] =
    firstOf(objectValue, stem)

  def anyUri: RDFParser[String] = (n,rdf) => n match {
    case DatatypeLiteral(str,iri) if iri == xsd_anyUri => Success(str)
    case _ => parseFail(s"Expected typed literal with datatype xsd:anyUri. Obtained: $n")
  }

  def oneOf: RDFParser[OneOf] = (n, rdf) => for {
    _ <- checkType(sx_OneOf)(n,rdf)
    optMin <- opt(sx_min,integer)(n,rdf)
    optMax <- opt(sx_max,max)(n,rdf)
    expressions <- arc(sx_expressions,tripleExpressionList2Plus)(n,rdf)
    semActs <- opt(sx_semActs, semActList1Plus)(n,rdf)
    annotations <- star(sx_annotation,annotationParser)(n,rdf)
  } yield OneOf(mkId(n),expressions,optMin,optMax,semActs,ls2Option(annotations))

  def eachOf: RDFParser[EachOf] = (n, rdf) => for {
    _ <- checkType(sx_EachOf)(n,rdf)
    optMin <- opt(sx_min,integer)(n,rdf)
    optMax <- opt(sx_max,max)(n,rdf)
    expressions <- arc(sx_expressions,tripleExpressionList2Plus)(n,rdf)
    semActs <- opt(sx_semActs, semActList1Plus)(n,rdf)
    annotations <- star(sx_annotation,annotationParser)(n,rdf)
  } yield EachOf(mkId(n),expressions,optMin,optMax,semActs,ls2Option(annotations))

  def ls2Option[A](ls: List[A]): Option[List[A]] =
    if (ls.isEmpty) None else Some(ls)

  def annotationParser: RDFParser[Annotation] = (n,rdf) => for {
   _ <- checkType(sx_Annotation)(n,rdf)
   pred <- arc(sx_predicate, iri)(n,rdf)
   obj <- arc(sx_object,objectValue)(n,rdf)
  } yield Annotation(pred,obj)

  def objectValue: RDFParser[ObjectValue] = (n,rdf) => n match {
    case iri: IRI => Success(IRIValue(iri))
    case StringLiteral(str) => Success(StringValue(str))
    case DatatypeLiteral(str,iri)=> Success(DatatypeString(str,iri))
    case LangLiteral(lex,lan) => Success(LangString(lex,lan.lang))
    case _ => parseFail(s"Unexpected object value: $n must be an IRI or a Literal")
  }

  def max: RDFParser[Max] = (n,rdf) => n match {
    case IntegerLiteral(n) => Success(IntMax(n))
    case StringLiteral("*") => Success(Star)
    case _ => parseFail(s"Unexpected node parsing max cardinality: $n")
  }

  def tripleExpressionList2Plus : RDFParser[List[TripleExpr]] =
    list2Plus(tripleExpression)

  def semActList1Plus: RDFParser[List[SemAct]] =
    list1Plus(semAct)

  def shapeExprList2Plus: RDFParser[List[ShapeExpr]] =
    list2Plus(shapeExpr)

  def shapeExprList1Plus: RDFParser[List[ShapeExpr]] =
    list1Plus(shapeExpr)

  def valueSetValueList1Plus: RDFParser[List[ValueSetValue]] =
    list1Plus(valueSetValue)

}

object RDF2ShEx extends RDF2ShEx {

 def rdf2Schema(rdf: RDFReader): Either[String,Schema] =
   getSchema(rdf)

 def tryRDF2Schema(rdf: RDFReader): Try[Schema] =
    getSchema(rdf) match {
      case Left(str) => Failure(new Exception(s"fromRDF error: $str"))
      case Right(s) => Success(s)
    }


}