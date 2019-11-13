package es.weso.shapeMaps
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.PREFIXES._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import es.weso.rdf.path._
import es.weso.shapeMaps.Parser._
import es.weso.shapeMaps.parser.ShapeMapParser.{StringContext => ShapeMapStringContext, _}
import es.weso.shapeMaps.parser._
import es.weso.utils.{EitherUtils, FileUtils}
import io.circe.Json

import scala.jdk.CollectionConverters._

/**
 * Visits the AST and builds the corresponding ShapeMaps classes
 */
class ShapeMapsMaker(
  base: Option[IRI],
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap = PrefixMap.empty) extends ShapeMapBaseVisitor[Any] with LazyLogging {

  val baseIRI = base.getOrElse(IRI(FileUtils.currentFolderURL))

  override def visitShapeMap(ctx: ShapeMapContext): Builder[QueryShapeMap] = for {
    associations <- visitList(visitPair, ctx.pair)
  } yield QueryShapeMap(associations, nodesPrefixMap, shapesPrefixMap)

  override def visitPair(ctx: PairContext): Builder[Association] = for {
    nodeSelector <- visitNodeSelector(ctx.nodeSelector)
    pair <- visitStatusAndShape(ctx.statusAndShape)
    reason <-visitReason(ctx.reason())
    appInfo <- visitJsonAttributes(ctx.jsonAttributes())
  } yield {
    val (shapeLabel, status) = pair
    val info = Info(status = status, reason = reason, appInfo = appInfo)
    Association(nodeSelector, shapeLabel, info)
  }

  override def visitStatusAndShape(ctx: StatusAndShapeContext): Builder[(ShapeMapLabel, Status)] = ctx match {
    case _ if isDefined(ctx.AT_START()) => ok((Start, Conformant))
    case _ if isDefined(ctx.shapeSelector) => for {
      status <- visitStatusAndShapeInfo(ctx)
      label <- visitShapeSelector(ctx.shapeSelector)
    } yield ((label,status))
    case _ if isDefined(ctx.ATPNAME_NS()) => err(s"visitStatusAndShape. ATPNAME_NS ${ctx.ATPNAME_NS().getText}")
    case _ if isDefined(ctx.ATPNAME_LN()) => for {
      iri <- resolve(ctx.ATPNAME_LN().getText.substring(1), shapesPrefixMap)
    } yield ((IRILabel(iri), Conformant))
    case _ => err(s"visitStatusAndShape: Unsupported case $ctx")
  }

  private def visitStatusAndShapeInfo(ctx: StatusAndShapeContext): Builder[Status] =
    if (isDefined(ctx.status())) {
      visitStatus(ctx.status)
    } else {
      ok(Conformant)
    }

  override def visitShapeSelector(ctx: ShapeSelectorContext): Builder[ShapeMapLabel] = ctx match {
    case _ if isDefined(ctx.shapeIri()) => for {
      iri <- visitShapeIri(ctx.shapeIri(), shapesPrefixMap)
    } yield IRILabel(iri)
    case _ if isDefined(ctx.KW_START()) => ok(Start)
    case _ => err(s"Internal error visitShapeLabel: unknown ctx $ctx")
  }

  override def visitReason(ctx: ReasonContext): Builder[Option[String]] =
    if (isDefined(ctx)) for {
      str <- visitString(ctx.string())
    } yield Some(str)
    else ok(None)

  override def visitJsonAttributes(ctx: JsonAttributesContext): Builder[Option[Json]] =
    if (isDefined(ctx)) ok(Some(Json.Null)) // TODO
    else ok(None)

  override def visitNodeSelector(ctx: NodeSelectorContext): Builder[NodeSelector] = ctx match {
    case _ if isDefined(ctx.objectTerm()) => for {
      node <- visitObjectTerm(ctx.objectTerm())
    } yield RDFNodeSelector(node)
    case _ if isDefined(ctx.triplePattern()) => visitTriplePattern(ctx.triplePattern())
    case _ if isDefined(ctx.extended()) => visitExtended(ctx.extended())
    case _ => err(s"Internal error visitNodeSelector: unknown ctx $ctx")
  }

  override def visitExtended(ctx: ExtendedContext): Builder[NodeSelector] = ctx match {
    case _ if isDefined(ctx.KW_SPARQL) => for {
      str <- visitString(ctx.string)
    } yield SparqlSelector(str)
    case _ if isDefined(ctx.nodeIri) => for {
      iri <- visitNodeIri(ctx.nodeIri, nodesPrefixMap)
      str <- visitString(ctx.string)
    } yield GenericSelector(iri,str)
    case _ => err(s"Internal error visitExtended: unknoen ctx $ctx")
  }

  // TODO: It would be safer to check that the first and last characters are indeed backquotes
  /* private def removeBackQuotes(str: String): String =
    str.substring(1,str.length - 1) */

  override def visitSubjectTerm(ctx: SubjectTermContext): Builder[RDFNode] = ctx match {
    case _ if isDefined(ctx.nodeIri()) => for {
      iri <- visitNodeIri(ctx.nodeIri(), nodesPrefixMap)
    } yield iri
    case _ if isDefined(ctx.rdfType()) => ok(`rdf:type`)
  }

  override def visitObjectTerm(ctx: ObjectTermContext): Builder[RDFNode] = ctx match {
    case _ if isDefined(ctx.subjectTerm()) => visitSubjectTerm(ctx.subjectTerm())
    case _ if isDefined(ctx.literal()) => for {
      literal <- visitLiteral(ctx.literal())
    } yield literal
  }

  def visitTriplePattern(ctx: TriplePatternContext): Builder[TriplePattern] = ctx match {
    case s: FocusSubjectContext => for {
      path <- visitPath(s.path())
      objectPattern <- if (isDefined(s.objectTerm())) for {
        obj <- visitObjectTerm(s.objectTerm())
      } yield NodePattern(obj)
      else ok(WildCard)
    } yield TriplePattern(Focus, path, objectPattern)
    case s: FocusObjectContext => for {
      path <- visitPath(s.path())
      subjectPattern <- if (isDefined(s.subjectTerm())) for {
        subj <- visitSubjectTerm(s.subjectTerm())
      } yield NodePattern(subj)
      else ok(WildCard)
    } yield TriplePattern(subjectPattern, path, Focus)
  }

  override def visitPath(ctx: PathContext): Builder[SHACLPath] =
    visitPathAlternative(ctx.pathAlternative())

  override def visitPathAlternative(ctx: PathAlternativeContext): Builder[SHACLPath] =
    for {
      alts <- {
        val r: List[Builder[SHACLPath]] = ctx.pathSequence().asScala.map(visitPathSequence(_)).toList
        EitherUtils.sequence(r) // .sequence
      }
    } yield if (alts.length == 1) alts.head
    else AlternativePath(alts)

  override def visitPathSequence(ctx: PathSequenceContext): Builder[SHACLPath] =
    for {
      seqs <- {
        val r: List[Builder[SHACLPath]] = ctx.pathEltOrInverse().asScala.map(visitPathEltOrInverse(_)).toList
        EitherUtils.sequence(r) //
      }
    } yield if (seqs.length == 1) seqs.head
    else SequencePath(seqs)

  override def visitPathEltOrInverse(ctx: PathEltOrInverseContext): Builder[SHACLPath] = for {
    pathElt <- visitPathElt(ctx.pathElt())
  } yield {
    if (isDefined(ctx.inverse())) InversePath(pathElt)
    else pathElt
  }

  // TODO: pathMod
  override def visitPathElt(ctx: PathEltContext): Builder[SHACLPath] = for {
    pathPrimary <- visitPathPrimary(ctx.pathPrimary())
  } yield {
    if (isDefined(ctx.pathMod())) {
      ctx.pathMod() match {
        case _: StarContext => ZeroOrMorePath(pathPrimary)
        case _: OptionalContext => ZeroOrOnePath(pathPrimary)
        case _: PlusContext => OneOrMorePath(pathPrimary)
      }
    } else pathPrimary
  }

  override def visitPathPrimary(ctx: PathPrimaryContext): Builder[SHACLPath] = ctx match {
    case _ if isDefined(ctx.nodeIri()) => visitNodeIri(ctx.nodeIri(), nodesPrefixMap).map(PredicatePath(_))
    case _ if isDefined(ctx.rdfType()) => ok(PredicatePath(`rdf:type`))
  }


  override def visitStatus(ctx: StatusContext): Builder[Status] = ctx match {
    case _ if (isDefined(ctx.negation)) => ok(NonConformant)
    case _ if (isDefined(ctx.questionMark)) => ok(Undefined)
    case _ => err(s"Internal error visitStatus: undefined ctx $ctx")
  }

  override def visitLiteral(ctx: LiteralContext): Builder[Literal] = {
    ctx match {
      case _ if (isDefined(ctx.rdfLiteral)) => visitRdfLiteral(ctx.rdfLiteral())
      case _ if (isDefined(ctx.numericLiteral)) => visitNumericLiteral(ctx.numericLiteral())
      case _ if (isDefined(ctx.booleanLiteral)) => visitBooleanLiteral(ctx.booleanLiteral())
      case _ => err(s"Internal error visitLiteral: unknown ctx $ctx")
    }
  }
  override def visitRdfLiteral(ctx: RdfLiteralContext): Builder[Literal] = {
    val str = visitString(ctx.string())
    /*  Language tagged literals disabled until we fix the grammar (see issue #48
    if (isDefined(ctx.LANGTAG())) {
      // We get the langTag and remove the first character (@)
      val lang = Lang(ctx.LANGTAG().getText().substring(1))
      str.map(s => LangLiteral(s, lang))
    } else */ if (isDefined(ctx.datatype)) {
      for {
        s <- str
        d <- visitDatatype(ctx.datatype(), nodesPrefixMap)
      } yield DatatypeLiteral(s, d)
    } else {
      str.map(StringLiteral(_))
    }
  }

  override def visitNumericLiteral(ctx: NumericLiteralContext): Builder[Literal] = {
    ctx match {
      case _ if (isDefined(ctx.INTEGER())) =>
        ok(IntegerLiteral(Integer.parseInt(ctx.INTEGER().getText)))
      case _ if (isDefined(ctx.DECIMAL())) =>
        ok(DecimalLiteral(BigDecimal(ctx.DECIMAL().getText)))
      case _ if (isDefined(ctx.DOUBLE())) => {
        val str = ctx.DOUBLE().getText
        ok(DoubleLiteral(str.toDouble))
      }
      case _ => err("Unknown ctx in numericLiteral")
    }
  }

  override def visitString(ctx: ShapeMapStringContext): Builder[String] = {
    if (isDefined(ctx.STRING_LITERAL_LONG1())) {
      ok(stripStringLiteralLong1(ctx.STRING_LITERAL_LONG1().getText()))
    } else if (isDefined(ctx.STRING_LITERAL_LONG2())) {
      ok(stripStringLiteralLong2(ctx.STRING_LITERAL_LONG2().getText()))
    } else if (isDefined(ctx.STRING_LITERAL1())) {
      ok(stripStringLiteral1(ctx.STRING_LITERAL1().getText()))
    } else if (isDefined(ctx.STRING_LITERAL2())) {
      ok(stripStringLiteral2(ctx.STRING_LITERAL2().getText()))
    } else
      err(s"visitString: Unknown ctx ${ctx.getClass.getName}")
  }

  def stripStringLiteral1(s: String): String = {
    s.substring(1,s.length - 1)
  }

  def stripStringLiteral2(s: String): String = {
    s.substring(1,s.length - 1)
  }

  def stripStringLiteralLong1(s: String): String = {
    s.substring(3, s.length() - 3)
  }

  def stripStringLiteralLong2(s: String): String = {
    s.substring(3,s.length - 3)
  }

  def visitDatatype(ctx: DatatypeContext, prefixMap: PrefixMap): Builder[IRI] = {
    visitNodeIri(ctx.nodeIri(), prefixMap)
  }

  def getBase: Builder[Option[IRI]] = ok(Some(baseIRI))

  private def visitNodeIri(ctx: NodeIriContext, prefixMap: PrefixMap): Builder[IRI] =
    if (isDefined(ctx.IRIREF())) for {
      base <- getBase
    } yield extractIRIfromIRIREF(ctx.IRIREF().getText, base)
    else for {
      prefixedName <- visitPrefixedName(ctx.prefixedName())
      iri <- resolve(prefixedName, prefixMap)
    } yield iri

  private def visitShapeIri(ctx: ShapeIriContext, prefixMap: PrefixMap): Builder[IRI] =
    if (isDefined(ctx.IRIREF())) for {
      base <- getBase
    } yield extractIRIfromIRIREF(ctx.IRIREF().getText, base)
    else for {
      prefixedName <- visitPrefixedName(ctx.prefixedName())
      iri <- resolve(prefixedName, prefixMap)
    } yield iri

  def resolve(prefixedName: String, prefixMap: PrefixMap): Builder[IRI] = {
    val (prefix, local) = splitPrefix(prefixedName)
    // logger.info(s"Resolve. prefix: $prefix local: $local Prefixed name: $prefixedName")
    prefixMap.getIRI(prefix) match {
      case None =>
        err(s"Prefix $prefix not found in current prefix map $prefixMap")
      case Some(iri) =>
        ok(iri + local)
    }
  }

  def splitPrefix(str: String): (String, String) = {
    if (str contains ':') {
      val (prefix, name) = str.splitAt(str.lastIndexOf(':'))
      (prefix, name.tail)
    } else {
      ("", str)
    }
  }

  override def visitPrefixedName(ctx: PrefixedNameContext): Builder[String] = {
    ok(ctx.getText())
    /*    ctx match {
          case _ if isDefined(ctx.PNAME_LN()) => ok(ctx.PNAME_LN().getText())
          case _ if isDefined(ctx.PNAME_NS()) => ok(ctx.PNAME_NS().getText())
          case _ => err("visitPrefixedName: Unknown value")
        } */
  }

  /*   override def visitNodeConstraintGroup(ctx: NodeConstraintGroupContext): Builder[ShapeExpr] =
       for {
        shapeOrRef <- visitShapeOrRef(ctx.shapeOrRef())
      } yield shapeOrRef */

  def extractIRIfromIRIREF(d: String, base: Option[IRI]): IRI = {
    val iriRef = "^<(.*)>$".r
    d match {
      case iriRef(i) => {
        base match {
          case None => IRI(i)
          case Some(b) => b.resolve(IRI(i))
        }
      }
    }
  }

  def getInteger(str: String): Builder[Int] = {
    try {
      ok(str.toInt)
    } catch {
      case _: NumberFormatException =>
        err(s"Cannot get integer from $str")
    }
  }

  def getDecimal(str: String): Builder[BigDecimal] = {
    try {
      ok(BigDecimal(str))
    } catch {
      case _: NumberFormatException =>
        err(s"Cannot get decimal from $str")
    }
  }

  def getDouble(str: String): Builder[Double] = {
    try {
      ok(str.toDouble)
    } catch {
      case _: NumberFormatException =>
        err(s"Cannot get double from $str")
    }
  }

  override def visitBooleanLiteral(ctx: BooleanLiteralContext): Builder[Literal] = {
    if (isDefined(ctx.KW_TRUE()))
      ok(BooleanLiteral(true))
    else
      ok(BooleanLiteral(false))
  }

  def isDefined[A](x: A): Boolean = x != null

  def visitList[A, B](
    visitFn: A => Builder[B],
    ls: java.util.List[A]): Builder[List[B]] = {
    EitherUtils.sequence(ls.asScala.toList.map(visitFn(_)))
  }

  def visitOpt[A, B](
    visitFn: A => Builder[B],
    v: A): Builder[Option[B]] =
    if (isDefined(v)) visitFn(v).map(Some(_))
    else ok(None)

}
