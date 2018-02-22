package es.weso.shex.compact

import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.Prefix
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.shex._
import es.weso.shex.parser.ShExDocBaseVisitor
import es.weso.shex.compact.Parser._
import es.weso.shex.parser.ShExDocParser.{StringContext => ShExStringContext, _}
import es.weso.utils.StrUtils._
import scala.collection.JavaConverters._

/**
 * Visits the AST and builds the corresponding ShEx abstract syntax
 */
class SchemaMaker extends ShExDocBaseVisitor[Any] with LazyLogging {

  type Start = Option[ShapeExpr]
  type NotStartAction = Either[Start, (ShapeLabel, ShapeExpr)]
  type Cardinality = (Option[Int], Option[Max])
  type Directive = Either[(Prefix, IRI),  // Prefix decl
                   Either[IRI,            // Base decl
                          IRI             // Import decl
                     ]]

  val star = (Some(0), Some(Star))
  val plus = (Some(1), Some(Star))
  val optional = (Some(0), Some(IntMax(1)))

  override def visitShExDoc(
    ctx: ShExDocContext): Builder[Schema] = {
    for {
      directives <- visitList(visitDirective, ctx.directive())
      startActions <- visitStartActions(ctx.startActions())
      notStartAction <- visitNotStartAction(ctx.notStartAction())
      statements <- visitList(visitStatement, ctx.statement())

      prefixMap <- getPrefixMap
      base <- getBase
      start <- getStart
      shapeMap <- getShapesMap
      tripleExprMap <- getTripleExprMap
    } yield {
      val importIRIs = directives.collect {
        case Right(Right(iri)) => iri
      }
      Schema.empty.copy(
        prefixes = if (!prefixMap.isEmpty) Some(prefixMap) else None,
        base = base,
        startActs = startActions,
        start = start,
        shapes = if (!shapeMap.isEmpty) Some(shapesMap2List(shapeMap)) else None,
        tripleExprMap = if (!tripleExprMap.isEmpty) Some(tripleExprMap) else None,
        imports = importIRIs
      )
    }
  }

  def shapesMap2List(sm: ShapesMap): List[ShapeExpr] = {
    sm.map { case (lbl, se) => se.addId(lbl) }.toList
  }

  override def visitStatement(
    ctx: StatementContext): Builder[Unit] = ctx match {
    case _ if (isDefined(ctx.directive())) =>
      visitDirective(ctx.directive()).map(_ => ())
    case _ if (isDefined(ctx.notStartAction())) =>
      visitNotStartAction(ctx.notStartAction()).map(_ => ())
  }

  override def visitNotStartAction(ctx: NotStartActionContext): Builder[NotStartAction] =
    if (ctx == null) ok(Left(None))
    else ctx match {
      case _ if (isDefined(ctx.start())) => {
        for {
          s <- visitStart(ctx.start())
        } yield Left(s)
      }
      case _ if (isDefined(ctx.shapeExprDecl())) =>
        for {
          s <- visitShapeExprDecl(ctx.shapeExprDecl())
        } yield Right(s)
    }

  override def visitStartActions(ctx: StartActionsContext): Builder[Option[List[SemAct]]] = {
    if (isDefined(ctx)) {
      val r: List[Builder[SemAct]] =
        ctx.codeDecl().asScala.map(visitCodeDecl(_)).toList
      r.sequence.map(Some(_))
    } else ok(None)
  }

  override def visitStart(
    ctx: StartContext): Builder[Option[(ShapeExpr)]] = {
    // logger.info(s"Visiting start...$ctx")
    if (isDefined(ctx)) {
      // logger.info(s"is defined...")
      for {
        shapeExpr <- visitShapeExpression(ctx.shapeExpression())
        _ <- {
          // logger.info(s"Shape expression for start: $shapeExpr")
          updateStart(Some(shapeExpr))
        }
        // semActs <- visitSemanticActions(ctx.semanticActions())
      } yield {
        // logger.info(s"Shape expression for start: $shapeExpr")
        Some(shapeExpr)
      }
    } else
      ok(None)
  }

  override def visitSemanticActions(ctx: SemanticActionsContext): Builder[List[SemAct]] =
    for {
      ls <- visitList(visitCodeDecl, ctx.codeDecl())
    } yield {
      ls
    }

  override def visitCodeDecl(ctx: CodeDeclContext): Builder[SemAct] =
    for {
      iri <- visitIri(ctx.iri())
      code <- optBuilder(ctx.CODE()).map(opt => opt.map(_.getText()))
      str <- optMapBuilder(code, cleanCode)
    } yield SemAct(iri, str)

  def cleanCode(str: String): Builder[String] = {
    val codeRegex = "^\\{(.*)%\\}$".r
    str match {
      case codeRegex(c) => ok(unescapeCode(c))
      case _ => err(s"cleanCode: $str doesn't match regex $codeRegex")
    }
  }

  def optBuilder[A](v: A): Builder[Option[A]] =
    if (v == null)
      ok(None)
    else
      ok(Some(v))

  def optMapBuilder[A, B](
    x: Option[A],
    f: A => Builder[B]): Builder[Option[B]] =
    x match {
      case None => ok(None)
      case Some(v) => f(v).map(Some(_))
    }

  override def visitShapeExprDecl(ctx: ShapeExprDeclContext): Builder[(ShapeLabel, ShapeExpr)] =
    for {
      label <- visitShapeExprLabel(ctx.shapeExprLabel())
      shapeExpr <- obtainShapeExpr(ctx)
      _ <- addShape(label, shapeExpr)
    } yield (label, shapeExpr)

  def obtainShapeExpr(ctx: ShapeExprDeclContext): Builder[ShapeExpr] =
    if (isDefined(ctx.KW_EXTERNAL())) {
      // TODO: What happens if there are semantic actions after External??
      ok(ShapeExternal(None))
    } else
      // TODO: Obtain stringFacet*
      visitShapeExpression(ctx.shapeExpression())

  override def visitShapeExpression(ctx: ShapeExpressionContext): Builder[ShapeExpr] =
    visitShapeOr(ctx.shapeOr())

  override def visitInlineShapeExpression(
    ctx: InlineShapeExpressionContext): Builder[ShapeExpr] =
    visitInlineShapeOr(ctx.inlineShapeOr())

  override def visitInlineShapeOr(
    ctx: InlineShapeOrContext): Builder[ShapeExpr] = for {
    shapes <- {
      val r: List[Builder[ShapeExpr]] =
        ctx.inlineShapeAnd().asScala.map(visitInlineShapeAnd(_)).toList
      r.sequence
    }
  } yield mkShapeOr(shapes, Flatten)

  override def visitInlineShapeAnd(
    ctx: InlineShapeAndContext): Builder[ShapeExpr] = {
    for {
      shapes <- {
        val r: List[Builder[ShapeExpr]] =
          ctx.inlineShapeNot().asScala.map(visitInlineShapeNot(_)).toList
        r.sequence
      }
    } yield {
      mkShapeAnd(shapes, Flatten)
    }
  }

  override def visitInlineShapeNot(
    ctx: InlineShapeNotContext): Builder[ShapeExpr] = for {
    shapeAtom <- visitInlineShapeAtom(ctx.inlineShapeAtom())
  } yield if (isDefined(ctx.negation()))
    ShapeNot(None, shapeAtom)
  else
    shapeAtom

  override def visitShapeOr(ctx: ShapeOrContext): Builder[ShapeExpr] = for {
    shapes <- {
      val r: List[Builder[ShapeExpr]] =
        ctx.shapeAnd().asScala.map(visitShapeAnd(_)).toList
      r.sequence
    }
  } yield mkShapeOr(shapes, NO_Flatten)

  private def mkShapeAnd(shapes: List[ShapeExpr], flatten: Boolean): ShapeExpr = {
   val zero = List[ShapeExpr]()
   def next(c: ShapeExpr, rs: List[ShapeExpr]): List[ShapeExpr] = c match {
     case ShapeAnd(None,ss) => ss ++ rs
     case _ => c :: rs
   }
   lazy val flattenShapes = shapes.foldRight(zero)(next)
   mkShapeOp(if (flatten) flattenShapes else shapes, ShapeAnd)
  }

  private def mkShapeOr(shapes: List[ShapeExpr], flatten: Boolean): ShapeExpr = {
    val zero = List[ShapeExpr]()
    def next(c: ShapeExpr, rs: List[ShapeExpr]): List[ShapeExpr] = c match {
      case ShapeOr(None,ss) => ss ++ rs
      case _ => c :: rs
    }
    lazy val flattenShapes = shapes.foldRight(zero)(next)
    mkShapeOp(if (flatten) flattenShapes else shapes,ShapeOr)
  }


  private def mkShapeOp(shapes: List[ShapeExpr],
                        op: (Option[ShapeLabel], List[ShapeExpr]) => ShapeExpr): ShapeExpr = {
    if (shapes.length == 1) shapes.head
    else op(None, shapes)
  }

  override def visitShapeAnd(
    ctx: ShapeAndContext): Builder[ShapeExpr] = {
    for {
      shapes <- {
        val r: List[Builder[ShapeExpr]] =
          ctx.shapeNot().asScala.map(visitShapeNot(_)).toList
        r.sequence
      }
    } yield {
      mkShapeAnd(shapes, NO_Flatten)
    }
  }

  private val NO_Flatten = false
  private val Flatten = true


  override def visitShapeNot(
    ctx: ShapeNotContext): Builder[ShapeExpr] = for {
    shapeAtom <- visitShapeAtom(ctx.shapeAtom())
  } yield if (isDefined(ctx.negation()))
    ShapeNot(None, shapeAtom)
  else shapeAtom

  private def visitShapeAtom(ctx: ShapeAtomContext): Builder[ShapeExpr] = {
    ctx match {
      case s: ShapeAtomNodeConstraintContext =>
        for {
          nk <- visitNodeConstraint(s.nodeConstraint())
          sr <- visitOpt(visitShapeOrRef, s.shapeOrRef())
        } yield sr match {
          case None => nk
          case Some(s) => ShapeAnd(None, List(nk, s))
        }

      case s: ShapeAtomShapeOrRefContext =>
        visitShapeOrRef(s.shapeOrRef())

      case s: ShapeAtomShapeExpressionContext =>
        visitShapeExpression(s.shapeExpression())

      case _: ShapeAtomAnyContext =>
        ok(ShapeExpr.any)

      case _ => err(s"Internal error visitShapeAtom: unknown ctx $ctx")
    }
  }

  def visitNodeConstraint(ctx: NodeConstraintContext): Builder[NodeConstraint] = {
    ctx match {
      case s: NodeConstraintLiteralContext =>
        for {
          xsFacets <- visitList(visitXsFacet, s.xsFacet())
        } yield NodeConstraint.nodeKind(LiteralKind, xsFacets)
      case s: NodeConstraintNonLiteralContext =>
        for {
          nodeKind <- visitNonLiteralKind(s.nonLiteralKind())
          stringFacets <- visitList(visitStringFacet, s.stringFacet())
        } yield NodeConstraint.nodeKind(nodeKind, stringFacets)
      case s: NodeConstraintDatatypeContext =>
        for {
          datatype <- visitDatatype(s.datatype())
          xsFacets <- visitList(visitXsFacet, s.xsFacet())
        } yield NodeConstraint.datatype(datatype, xsFacets)
      case c: NodeConstraintValueSetContext =>
        for {
          vs <- visitValueSet(c.valueSet())
          xsFacets <- visitList(visitXsFacet, c.xsFacet())
        } yield NodeConstraint.valueSet(vs, xsFacets)

      case c: NodeConstraintFacetContext => for {
        facets <- visitList(visitXsFacet, c.xsFacet)
      } yield NodeConstraint.empty.copy(
        xsFacets = facets)
    }
  }

  def visitInlineShapeAtom(ctx: InlineShapeAtomContext): Builder[ShapeExpr] = {
    ctx match {
      case s: InlineShapeAtomNodeConstraintContext =>
        for {
          nk <- visitNodeConstraint(s.nodeConstraint())
          sr <- visitOpt(visitInlineShapeOrRef, s.inlineShapeOrRef())
        } yield sr match {
          case None => nk
          case Some(s) => {
            ShapeAnd(None, List(nk, s))
          }
        }

      case s: InlineShapeAtomShapeOrRefContext =>
        for {
          sr <- visitInlineShapeOrRef(s.inlineShapeOrRef())
          nk <- visitOpt(visitNodeConstraint, s.nodeConstraint())
        } yield nk match {
          case None => sr
          case Some(n) => ShapeAnd(None, List(sr, n))
        }

      case s: InlineShapeAtomShapeExpressionContext =>
        visitShapeExpression(s.shapeExpression())

      case _: InlineShapeAtomAnyContext =>
        ok(ShapeExpr.any)
    }
  }

  override def visitValueSet(
    ctx: ValueSetContext): Builder[List[ValueSetValue]] = {
    visitList(visitValueSetValue, ctx.valueSetValue())
  }

  private def nonEmpty[A](ls: java.util.List[A]): Boolean = ls.size > 0

/*  private def valuetoLiteralStemRangeValue(v: ValueSetValue): Builder[LiteralStemRangeValueObject] = v match {
    case ol: ObjectLiteral => ok(LiteralStemRangeValueObject(ol))
    case LiteralStem(ol) => ok(LiteralStemRangeValueObject(ol))
    case _ => err(s"Cannot convert value object $v to LiteralStemRangeValue")
  } */

  override def visitValueSetValue(ctx: ValueSetValueContext): Builder[ValueSetValue] = {
    ctx match {
      case _ if isDefined(ctx.iriRange()) => visitIriRange(ctx.iriRange())
      case _ if isDefined(ctx.literalRange()) => visitLiteralRange(ctx.literalRange())
      case _ if isDefined(ctx.languageRange()) => visitLanguageRange(ctx.languageRange())
      case _ if nonEmpty(ctx.iriExclusion()) => {
        for {
          exclusions <- visitList(visitIriExclusion, ctx.iriExclusion())
        } yield IRIStemRange(IRIStemWildcard(),Some(exclusions))
      }
      case _ if nonEmpty(ctx.literalExclusion()) => {
        for {
          exclusions <- visitList(visitLiteralExclusion, ctx.literalExclusion())
        } yield LiteralStemRange(LiteralStemRangeWildcard(),Some(exclusions))
      }
      case _ if nonEmpty(ctx.languageExclusion()) => for {
        exclusions <- visitList(visitLanguageExclusion, ctx.languageExclusion())
      } yield LanguageStemRange(LanguageStemRangeWildcard(),Some(exclusions))
      case _ => err(s"visitValueSetValue: Unknown value")
    }
  }

  override def visitLiteralRange(ctx: LiteralRangeContext): Builder[ValueSetValue] = {
    for {
      literal <- visitLiteral(ctx.literal())
      exclusions <- visitList(visitLiteralExclusion, ctx.literalExclusion)
      str <- if (isDefined(ctx.STEM_MARK())) value2String(literal)
             else ok("Unused string") // Unused string...if no STEM_MARK, returns the literal
    } yield if (!isDefined(ctx.STEM_MARK())) literal
    else { // it is a stem
      if (exclusions.isEmpty) {
        LiteralStem(str)
      } else
        LiteralStemRange(LiteralStemRangeString(str), Some(exclusions))
    }
  }

  override def visitLanguageRange(ctx: LanguageRangeContext): Builder[ValueSetValue] = {
    for {
      es <- visitList(visitLanguageExclusion, ctx.languageExclusion)
    } yield {
      // TODO Add language exclusion
      val lang = getLanguage(ctx.LANGTAG().getText())
      if (!isDefined(ctx.STEM_MARK())) {
        Language(lang)
      } else
      if (es.isEmpty) LanguageStem(lang)
      else LanguageStemRange(LanguageStemRangeLang(lang), Some(es))
    }
  }

  override def visitIriExclusion(ctx: IriExclusionContext): Builder[IRIExclusion] = for {
    iri <- visitIri(ctx.iri())
  } yield if (isDefined(ctx.STEM_MARK()))
     IRIStemExclusion(IRIStem(iri))
    else
     IRIRefExclusion(iri)

  override def visitLanguageExclusion(ctx: LanguageExclusionContext): Builder[LanguageExclusion] = {
    val lang = getLanguage(ctx.LANGTAG().getText())
    ok(if (isDefined(ctx.STEM_MARK()))
         LanguageStemExclusion(LanguageStem(lang))
       else LanguageTagExclusion(lang)
    )
  }

  private def value2String(v: ValueSetValue): Builder[String] = v match {
    case StringValue(str) => ok(str)
    case _ => err(s"Cannot convert value $v to string")
  }

  override def visitLiteralExclusion(ctx: LiteralExclusionContext): Builder[LiteralExclusion] = for {
    literal <- visitLiteral(ctx.literal())
    str <- value2String(literal)
  } yield
     if(isDefined(ctx.STEM_MARK())) {
       LiteralStemExclusion(LiteralStem(str))
     }
     else LiteralStringExclusion(str)

  override def visitIriRange(ctx: IriRangeContext): Builder[ValueSetValue] = for {
    iri <- visitIri(ctx.iri())
    exclusions <- visitList(visitIriExclusion, ctx.iriExclusion())
  } yield {
    if (!isDefined(ctx.STEM_MARK())) IRIValue(iri)
    else if (exclusions.isEmpty)
      IRIStem(iri)
    else {
      IRIStemRange(IRIStemValueIRI(iri), Some(exclusions))
    }
  }

  override def visitLiteral(ctx: LiteralContext): Builder[ValueSetValue] = {
    ctx match {
      case _ if isDefined(ctx.rdfLiteral()) => visitRdfLiteral(ctx.rdfLiteral())
      case _ if isDefined(ctx.numericLiteral()) => for {
        nl <- visitNumericLiteral(ctx.numericLiteral())
        v <- numericLiteral2ValueObject(nl)
      } yield v
      case _ if isDefined(ctx.booleanLiteral()) => visitBooleanLiteral(ctx.booleanLiteral())
      case _ => err(s"visitLiteral: Unknown ${ctx}")
    }
  }

  private def getNumericLiteralOrNumericDatatype(ctx: NumericFacetContext): Builder[NumericLiteral] = ctx match {
    case _ if isDefined(ctx.numericLiteral()) => visitNumericLiteral(ctx.numericLiteral())
    case _ if isDefined(ctx.string()) => for {
      str <- visitString(ctx.string())
      dt <- visitDatatype(ctx.datatype())
      nl <- dt match {
        case `xsd_integer` => ok(NumericInt(Integer.parseInt(str)))
        case `xsd_double` => ok(NumericDecimal(str.toDouble,str))
        case `xsd_decimal` => ok(NumericDecimal(BigDecimal(str),str))
        case _ => err(s"Unsupported numericFacet of string $str with datatype $dt")
      }
    } yield nl
  }

    private def numericLiteral2ValueObject(nl: NumericLiteral): Builder[ValueSetValue] = {
    nl match {
      case NumericInt(n) => ok(ObjectValue.intValue(n))
      case NumericDouble(d,repr) => ok(ObjectValue.doubleValue(d,repr))
      case NumericDecimal(d,repr) => ok(ObjectValue.decimalValue(d,repr))
    }
  }

  override def visitRdfLiteral(
    ctx: RdfLiteralContext): Builder[ValueSetValue] = {
    val str = visitString(ctx.string())
    if (isDefined(ctx.LANGTAG())) {
      val lang = getLanguage(ctx.LANGTAG().getText())
      str.map(s => LangString(s, lang))
    } else if (isDefined(ctx.datatype)) {
      for {
        s <- str
        d <- visitDatatype(ctx.datatype())
      } yield DatatypeString(s, d)
    } else {
      str.map(StringValue(_))
    }
  }

  private def okStringLiteral(str:String): Builder[String] =
    ok(unescapeStringLiteral(str))

  override def visitString(
    ctx: ShExStringContext): Builder[String] = {
    if (isDefined(ctx.STRING_LITERAL_LONG1())) {
      okStringLiteral(stripStringLiteralLong1(ctx.STRING_LITERAL_LONG1().getText()))
    } else if (isDefined(ctx.STRING_LITERAL_LONG2())) {
      okStringLiteral(stripStringLiteralLong2(ctx.STRING_LITERAL_LONG2().getText()))
    } else if (isDefined(ctx.STRING_LITERAL1())) {
      okStringLiteral(stripStringLiteral1(ctx.STRING_LITERAL1().getText()))
    } else if (isDefined(ctx.STRING_LITERAL2())) {
      okStringLiteral(stripStringLiteral2(ctx.STRING_LITERAL2().getText()))
    } else
      err(s"visitString: Unknown ctx ${ctx.getClass.getName}")
  }

  def stripStringLiteral1(s: String): String = {
    val regexStr = "\'(.*)\'".r
    s match {
      case regexStr(s) => s
      case _ => throw new Exception(s"stripStringLiteral2 $s doesn't match regex")
    }
  }

  def stripStringLiteral2(s: String): String = {
    val regexStr = "\"(.*)\"".r
    s match {
      case regexStr(s) => s
      case _ => throw new Exception(s"stripStringLiteral2 $s doesn't match regex")
    }
  }

  def stripStringLiteralLong1(s: String): String = {
    val regexStr = "\'\'\'(.*)\'\'\'".r
    s match {
      case regexStr(s) => s
      case _ => throw new Exception(s"stripStringLiteralLong1 $s doesn't match regex")
    }
  }

  def stripStringLiteralLong2(s: String): String = {
    val regexStr = "\"\"\"(.*)\"\"\"".r
    s match {
      case regexStr(s) => s
      case _ => throw new Exception(s"stripStringLiteralLong1 $s doesn't match regex")
    }
  }

  override def visitDatatype(ctx: DatatypeContext): Builder[IRI] = {
    visitIri(ctx.iri())
  }

  override def visitIri(ctx: IriContext): Builder[IRI] =
    if (isDefined(ctx.IRIREF())) for {
      base <- getBase
    } yield extractIRIfromIRIREF(ctx.IRIREF().getText, base)
    else for {
      prefixedName <- visitPrefixedName(ctx.prefixedName())
      iri <- resolve(prefixedName)
    } yield iri

  def resolve(prefixedName: String): Builder[IRI] = {
    val (prefix, local) = splitPrefix(prefixedName)
    // logger.info(s"Resolve. prefix: $prefix local: $local Prefixed name: $prefixedName")
    getPrefixMap.flatMap(prefixMap =>
      prefixMap.getIRI(prefix) match {
        case None =>
          err(s"Prefix $prefix not found in current prefix map $prefixMap")
        case Some(iri) =>
          ok(iri + local)
      })
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
    val str = unescapeIRI(d)
    val iriRef = "^<(.*)>$".r
    str match {
      case iriRef(i) => {
        // TODO: Check base declaration
        base match {
          case None => IRI(i)
          case Some(b) => b + i
        }
      }
    }
  }
  override def visitNumericLiteral(ctx: NumericLiteralContext): Builder[NumericLiteral] = {
    ctx match {
      case _ if (isDefined(ctx.INTEGER())) =>
        ok(NumericInt(Integer.parseInt(ctx.INTEGER().getText)))
      case _ if (isDefined(ctx.DECIMAL())) => {
        val repr = ctx.DECIMAL().getText
        ok(NumericDecimal(BigDecimal(repr), repr))
      }
      case _ if (isDefined(ctx.DOUBLE())) => {
        val repr = ctx.DOUBLE().getText
        ok(NumericDouble(repr.toDouble,repr))
      }
      case _ => err("Unknown ctx in numericLiteral")
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

  override def visitBooleanLiteral(
    ctx: BooleanLiteralContext): Builder[ValueSetValue] = {
    if (isDefined(ctx.KW_TRUE()))
      ok(DatatypeString("true", xsd_boolean))
    else
      ok(DatatypeString("false", xsd_boolean))
  }

  override def visitXsFacet(ctx: XsFacetContext): Builder[XsFacet] =
    ctx match {
      case _ if (isDefined(ctx.stringFacet())) =>
        visitStringFacet(ctx.stringFacet())
      case _ if (isDefined(ctx.numericFacet())) =>
        visitNumericFacet(ctx.numericFacet())
      case _ => err(s"visitXsFacet: Unsupported ${ctx.getClass.getName}")
    }

  override def visitStringFacet(
    ctx: StringFacetContext): Builder[XsFacet] = ctx match {
    case _ if (isDefined(ctx.stringLength())) => for {
      n <- getInteger(ctx.INTEGER().getText())
      stringLength <- visitStringLength(ctx.stringLength)(n)
    } yield stringLength
    case _ if (isDefined(ctx.REGEXP())) => {
      ok(Pattern(
        unescapePattern(removeSlashes(ctx.REGEXP().getText())),
        if (isDefined(ctx.REGEXP_FLAGS())) Some(ctx.REGEXP_FLAGS().getText())
        else None))
    }
/*    case _ if (isDefined(ctx.KW_PATTERN())) => for {
      str <- visitString(ctx.string())
    } yield Pattern(str, None) */
    case _ => err(s"visitStringFacet: Unsupported ${ctx.getClass.getName}")
  }

  private def unscapeSlashes(str:String): String = {
    str.replaceAllLiterally(s"\\/","/")
  }

  private def removeSlashes(str: String): String = {
    val slashedRegex = "/(.*)/".r
    str match {
      case slashedRegex(s) => unscapeSlashes(s)
      case _ => str
    }
  }
  override def visitStringLength(
    ctx: StringLengthContext): Int => Builder[StringFacet] = n => {
    if (isDefined(ctx.KW_LENGTH())) ok(Length(n))
    else if (isDefined(ctx.KW_MINLENGTH())) ok(MinLength(n))
    else if (isDefined(ctx.KW_MAXLENGTH())) ok(MaxLength(n))
    else
      err(s"visitStringLength: Unknown value for $ctx")
  }

  sealed trait NumericRange
  case object NRMinInclusive extends NumericRange
  case object NRMinExclusive extends NumericRange
  case object NRMaxInclusive extends NumericRange
  case object NRMaxExclusive extends NumericRange

  sealed trait NumericLength
  case object NLTotalDigits extends NumericLength
  case object NLFractionDigits extends NumericLength

  override def visitNumericFacet(ctx: NumericFacetContext): Builder[XsFacet] = {
    ctx match {
      case _ if isDefined(ctx.numericRange()) => for {
        nr <- visitNumericRange(ctx.numericRange())
        v <- getNumericLiteralOrNumericDatatype(ctx)
        nf <- makeNumericFacet(nr, v)
      } yield nf
      case _ if isDefined(ctx.numericLength()) => for {
        nl <- visitNumericLength(ctx.numericLength())
        n <- getInteger(ctx.INTEGER().getText)
        numLength <- makeNumericLength(nl, n)
      } yield numLength
      case _ => err("VisitNumericFacet. Unknown state for ctx")
    }
  }

  override def visitNumericRange(ctx: NumericRangeContext): Builder[NumericRange] = {
    ctx match {
      case _ if isDefined(ctx.KW_MININCLUSIVE()) => ok(NRMinInclusive)
      case _ if isDefined(ctx.KW_MINEXCLUSIVE()) => ok(NRMinExclusive)
      case _ if isDefined(ctx.KW_MAXINCLUSIVE()) => ok(NRMaxInclusive)
      case _ if isDefined(ctx.KW_MAXEXCLUSIVE()) => ok(NRMaxExclusive)
    }
  }

  override def visitNumericLength(ctx: NumericLengthContext): Builder[NumericLength] = {
    ctx match {
      case _ if isDefined(ctx.KW_TOTALDIGITS()) => ok(NLTotalDigits)
      case _ if isDefined(ctx.KW_FRACTIONDIGITS()) => ok(NLFractionDigits)
    }
  }

  def makeNumericLiteral(lexicalForm: String, datatype: IRI): Builder[NumericLiteral] = {
    datatype match {
      case `xsd_integer` => for {
        n <- getInteger(lexicalForm)
      } yield NumericInt(n)
      case `xsd_decimal` => for {
        d <- getDecimal(lexicalForm)
      } yield NumericDecimal(d, lexicalForm)
      case `xsd_double` => for {
        d <- getDouble(lexicalForm)
      } yield NumericDouble(d, lexicalForm)
      case `xsd_float` => for { // TODO: Check if floats and doubles are equivalent
        d <- getDouble(lexicalForm)
      } yield NumericDouble(d,lexicalForm)
      case _ => err(s"Numeric Literal '$lexicalForm' applied to unknown datatype $datatype ")
    }
  }

  def makeNumericFacet(nr: NumericRange, nl: NumericLiteral): Builder[NumericFacet] = {
    nr match {
      case NRMinInclusive => ok(MinInclusive(nl))
      case NRMinExclusive => ok(MinExclusive(nl))
      case NRMaxInclusive => ok(MaxInclusive(nl))
      case NRMaxExclusive => ok(MaxExclusive(nl))
    }
  }

  def makeNumericLength(nr: NumericLength, n: Int): Builder[NumericFacet] = {
    nr match {
      case NLTotalDigits => ok(TotalDigits(n))
      case NLFractionDigits => ok(FractionDigits(n))
    }
  }
  override def visitNodeConstraintLiteral(ctx: NodeConstraintLiteralContext): Builder[ShapeExpr] = for {
    facets <- visitList(visitXsFacet, ctx.xsFacet())
  } yield NodeConstraint.nodeKind(LiteralKind, facets)

  override def visitNodeConstraintNonLiteral(ctx: NodeConstraintNonLiteralContext): Builder[ShapeExpr] = for {
    nk <- visitNonLiteralKind(ctx.nonLiteralKind())
    facets <- visitList(visitStringFacet, ctx.stringFacet())
  } yield NodeConstraint.nodeKind(nk, facets)

  override def visitNonLiteralKind(ctx: NonLiteralKindContext): Builder[NodeKind] = {
    ctx match {
      case _ if isDefined(ctx.KW_IRI()) => ok(IRIKind)
      case _ if isDefined(ctx.KW_BNODE()) => ok(BNodeKind)
      case _ if isDefined(ctx.KW_NONLITERAL()) => ok(NonLiteralKind)
    }
  }

  override def visitInlineShapeOrRef(ctx: InlineShapeOrRefContext): Builder[ShapeExpr] = ctx match {
    case _ if (isDefined(ctx.inlineShapeDefinition())) =>
      visitInlineShapeDefinition(ctx.inlineShapeDefinition())
    case _ if (isDefined(ctx.shapeRef())) =>
      visitShapeRef(ctx.shapeRef())
    case _ => err(s"internal Error: visitShapeOrRef. Unknown $ctx")
  }

  override def visitShapeRef(ctx: ShapeRefContext): Builder[ShapeExpr] = ctx match {
    case _ if (isDefined(ctx.ATPNAME_NS())) => {
      val nameNS = ctx.ATPNAME_NS().getText().tail
      resolve(nameNS).map(iri => ShapeRef(IRILabel(iri)))
    }
    case _ if (isDefined(ctx.ATPNAME_LN())) => {
      val nameLN = ctx.ATPNAME_LN().getText().tail
      resolve(nameLN).map(iri => ShapeRef(IRILabel(iri)))
    }
    case _ if (isDefined(ctx.shapeExprLabel())) => for {
      lbl <- visitShapeExprLabel(ctx.shapeExprLabel())
    } yield ShapeRef(lbl)
  }

  override def visitShapeOrRef(ctx: ShapeOrRefContext): Builder[ShapeExpr] = ctx match {
    case _ if (isDefined(ctx.shapeDefinition())) =>
      visitShapeDefinition(ctx.shapeDefinition())
    case _ if (isDefined(ctx.shapeRef())) =>
      visitShapeRef(ctx.shapeRef())
    case _ => err(s"internal Error: visitShapeOrRef. Unknown $ctx")
  }

  override def visitInlineShapeDefinition(ctx: InlineShapeDefinitionContext): Builder[ShapeExpr] = {
    for {
      qualifiers <- visitList(visitQualifier, ctx.qualifier())
      tripleExpr <- visitOpt(visitTripleExpression,ctx.tripleExpression)
      shape <- makeShape(qualifiers, tripleExpr, List(), List())
    } yield shape
  }
  override def visitTripleExpression(ctx: TripleExpressionContext): Builder[TripleExpr] = {
    visitOneOfTripleExpr(ctx.oneOfTripleExpr())
  }

  override def visitShapeDefinition(ctx: ShapeDefinitionContext): Builder[ShapeExpr] = {
    for {
      qualifiers <- visitList(visitQualifier, ctx.qualifier())
      optTripleExpr <- visitOpt(visitTripleExpression,ctx.tripleExpression())
      semActs <- visitSemanticActions(ctx.semanticActions())
      anns <- visitList(visitAnnotation, ctx.annotation())
      // newTripleExpr <- addAnnotations(tripleExpr,anns)
      shape <- makeShape(qualifiers, optTripleExpr, semActs,anns)
    } yield shape
  }

  // TODO: Maybe remove the following method
  def addAnnotations(
    maybeTe: Option[TripleExpr],
    anns: List[Annotation]): Builder[Option[TripleExpr]] =
    maybeTe match {
      case None => ok(None)
      case Some(te) => if (anns.isEmpty) ok(maybeTe)
      else te match {
        case t: TripleConstraint => ok(Some(t.copy(annotations = Some(anns))))
        case so: OneOf => ok(Some(so.copy(annotations = Some(anns))))
        case eo: EachOf => ok(Some(eo.copy(annotations = Some(anns))))
        case _ => err(s"Can't add annotations $anns to $te")
      }
    }

  def makeShape(
    qualifiers: List[Qualifier],
    tripleExpr: Option[TripleExpr],
    semActs: List[SemAct],
    anns: List[Annotation]): Builder[ShapeExpr] = {
    val containsClosed =
      if (qualifiers.contains(Closed))
        Some(true)
      else
        Some(false)
    val ls: List[IRI] = qualifiers.map(_.getExtras).flatten
    val extras: Option[List[IRI]] =
      if (ls.isEmpty) None
      else Some(ls)
    val inheritList = qualifiers.map(_.getIncluded).flatten
    val shape = Shape.empty.copy(
      closed = containsClosed,
      extra = extras,
      expression = tripleExpr,
      semActs = if (semActs.isEmpty) None else Some(semActs),
      annotations = if (anns.isEmpty) None else Some(anns)
    )
    inheritList.length match {
      case 0 => ok(shape)
      case 1 => ok(shape.copy(inherit = Some(inheritList.head)))
      case _ => err(s"Inherit list $inheritList has more than one shapeLabel")
    }
  }

  override def visitQualifier(ctx: QualifierContext): Builder[Qualifier] = {
    ctx match {
      case _ if (isDefined(ctx.KW_CLOSED())) => ok(Closed)
      case _ if (isDefined(ctx.includeSet())) =>
        visitIncludeSet(ctx.includeSet())
      case _ if (isDefined(ctx.extraPropertySet())) =>
        visitExtraPropertySet(ctx.extraPropertySet())
    }
  }

  override def visitIncludeSet(ctx: IncludeSetContext): Builder[Qualifier] = for {
    sl <- visitList(visitTripleExprLabel, ctx.tripleExprLabel())
  } yield Include(sl)

  override def visitExtraPropertySet(ctx: ExtraPropertySetContext): Builder[Qualifier] = for {
    ls <- visitList(visitPredicate, ctx.predicate())
  } yield Extra(ls)

  override def visitOneOfTripleExpr(
    ctx: OneOfTripleExprContext): Builder[TripleExpr] = ctx match {
        case _ if (isDefined(ctx.groupTripleExpr())) => for {
          tripleExpr <- visitGroupTripleExpr(ctx.groupTripleExpr())
        } yield tripleExpr
        case _ if (isDefined(ctx.multiElementOneOf())) => for {
          tripleExpr <- visitMultiElementOneOf(ctx.multiElementOneOf())
        } yield tripleExpr
        case _ => err(s"visitOneOfShape: unknown $ctx")
  }

  override def visitGroupTripleExpr(
    ctx: GroupTripleExprContext): Builder[TripleExpr] = {
    ctx match {
      case _ if (isDefined(ctx.singleElementGroup())) =>
        visitSingleElementGroup(ctx.singleElementGroup())
      case _ if (isDefined(ctx.multiElementGroup())) =>
        visitMultiElementGroup(ctx.multiElementGroup())
      case _ => err(s"visitGroupTripleExpr: unknown $ctx")
    }
  }

  override def visitUnaryTripleExpr(ctx: UnaryTripleExprContext): Builder[TripleExpr] =
    ctx match {
      case _ if (isDefined(ctx.include())) =>
        visitInclude(ctx.include())
      case _ => for {
        maybeLbl <- visitOpt(visitTripleExprLabel, ctx.tripleExprLabel())
        te <- if (isDefined(ctx.bracketedTripleExpr()))
               visitBracketedTripleExpr(ctx.bracketedTripleExpr())
              else if (isDefined(ctx.tripleConstraint()))
               visitTripleConstraint(ctx.tripleConstraint())
              else err(s"visitUnaryTripleExpr: unknown $ctx")
        te1 <- maybeLbl match {
          case None => ok(te)
          case Some(lbl) => addTripleExprLabel(lbl,te)
        }
      } yield te1
    }

  // TODO: Where should I put ProductionLabels ?
  override def visitTripleExprLabel(ctx: TripleExprLabelContext): Builder[ShapeLabel] = ctx match {
    case _ if (isDefined(ctx.iri())) => visitIri(ctx.iri()).map(IRILabel(_))
    case _ if (isDefined(ctx.blankNode())) => visitBlankNode(ctx.blankNode()).map(BNodeLabel(_))
    case _ => err(s"Unknown tripelExprLabel")
  }


  override def visitTripleConstraint(
    ctx: TripleConstraintContext): Builder[TripleExpr] =
    for {
      sense <- visitSenseFlags(ctx.senseFlags())
      predicate <- visitPredicate(ctx.predicate())
      shapeExpr <- visitInlineShapeExpression(ctx.inlineShapeExpression())
      cardinality <- getCardinality(ctx.cardinality())
      semActs <- visitSemanticActions(ctx.semanticActions())
      anns <- visitList(visitAnnotation, ctx.annotation())
    } yield {
      TripleConstraint.
        emptyPred(predicate).copy(
          optInverse = sense.optInverse,
          optNegated = sense.optNegated,
          valueExpr = Some(shapeExpr),
          optMin = cardinality._1,
          optMax = cardinality._2,
          annotations = if (anns.isEmpty) None
          else Some(anns),
          semActs =
            if (semActs.isEmpty) None
            else Some(semActs))
    }
  case class Sense(
    optInverse: Option[Boolean],
    optNegated: Option[Boolean])

  override def visitSenseFlags(ctx: SenseFlagsContext): Builder[Sense] = {
    if (isDefined(ctx)) {
      val ls: List[String] = ctx.children.asScala.toList.map(_.getText)
      val optInverse = if (ls.contains("^")) Some(true) else None
      val optNegated = if (ls.contains("!")) Some(true) else None
      ok(Sense(optInverse, optNegated))
    } else
      ok(Sense(None, None))
  }

  def getCardinality(
    ctx: CardinalityContext): Builder[Cardinality] = {
    if (isDefined(ctx))
      ctx match {
        case _: StarCardinalityContext => ok(star)
        case _: PlusCardinalityContext => ok(plus)
        case _: OptionalCardinalityContext => ok(optional)
        case s: RepeatCardinalityContext => visitRepeatCardinality(s)
        case _ => err(s"Not implemented cardinality ${ctx.getClass.getName}")
      }
    else ok((None, None))
  }

  override def visitRepeatCardinality(ctx: RepeatCardinalityContext): Builder[Cardinality] = {
    visitRepeatRange(ctx.repeatRange())
  }

  private def visitRepeatRange(ctx: RepeatRangeContext): Builder[Cardinality] =
   ctx match {
     case s: ExactRangeContext => {
       getInteger(s.INTEGER().getText()).map(n =>
         (Some(n),Some(IntMax(n)))
       )
     }
     case s: MinMaxRangeContext => for {
       min <- visitMin_range(s.min_range())
       max <- visitMax_range(s.max_range())
     } yield max match {
           case None => (Some(min),Some(Star))
           case Some(m) => (Some(min),Some(m))
         }
   }

  override def visitMin_range(
    ctx: Min_rangeContext): Builder[Int] = {
    getInteger(ctx.INTEGER().getText())
  }

  override def visitMax_range(
    ctx: Max_rangeContext): Builder[Option[Max]] = {
    if (isDefined(ctx)) {
      if (isDefined(ctx.INTEGER())) {
        getInteger(ctx.INTEGER().getText()).map(n => Some(IntMax(n)))
      } else {
        // Asume star
        ok(Some(Star))
      }
    } else
      ok(None)
  }

  override def visitPredicate(
    ctx: PredicateContext): Builder[IRI] = {
    ctx match {
      case _ if (isDefined(ctx.iri())) =>
        visitIri(ctx.iri())
      case _ if (isDefined(ctx.rdfType())) =>
        ok(rdf_type)
    }
  }

  override def visitInclude(
    ctx: IncludeContext): Builder[TripleExpr] =
    for {
      lbl <- visitTripleExprLabel(ctx.tripleExprLabel())
    } yield Inclusion(lbl)

  override def visitBracketedTripleExpr(ctx: BracketedTripleExprContext): Builder[TripleExpr] =
    for {
      tripleExpr <- visitInnerTripleExpr(ctx.innerTripleExpr())
      cardinality <- getCardinality(ctx.cardinality())
      annotations <- visitList(visitAnnotation, ctx.annotation())
      semActs <- visitSemanticActions(ctx.semanticActions())
    } yield extendTripleExpr(tripleExpr, cardinality, annotations, semActs)

  def extendTripleExpr(
    te: TripleExpr,
    cardinality: Cardinality,
    anns: List[Annotation],
    sActs: List[SemAct]): TripleExpr =
    te match {
      case tc: TripleConstraint => tc.copy(
        optMin = cardinality._1,
        optMax = cardinality._2,
        annotations =
          if (anns.isEmpty) None
          else Some(anns),
        semActs =
          if (sActs.isEmpty) None
          else Some(sActs))
      case eo: EachOf => eo.copy(
        optMin = cardinality._1,
        optMax = cardinality._2,
        annotations =
          if (anns.isEmpty) None
          else Some(anns),
        semActs =
          if (sActs.isEmpty) None
          else Some(sActs))
      case so: OneOf => so.copy(
        optMin = cardinality._1,
        optMax = cardinality._2,
        annotations =
          if (anns.isEmpty) None
          else Some(anns),
        semActs =
          if (sActs.isEmpty) None
          else Some(sActs))
      case i: Inclusion =>
        // TODO: Check how to extend include
        i
    }

  override def visitAnnotation(ctx: AnnotationContext): Builder[Annotation] = for {
    pred <- visitPredicate(ctx.predicate())
    obj <- if (isDefined(ctx.iri())) {
      visitIri(ctx.iri()).map { iri =>
        {
          val o: ObjectValue = IRIValue(iri)
          o
        }
      }
    } else {
      visitLiteral(ctx.literal()).map(v => v match {
        case o: ObjectValue => o
        case _ => throw new Exception(s"Unknown value in annotation $v")
      })
    }
  } yield Annotation(pred, obj)

  override def visitInnerTripleExpr(ctx: InnerTripleExprContext): Builder[TripleExpr] =
    ctx match {
      case _ if isDefined(ctx.multiElementGroup()) => visitMultiElementGroup(ctx.multiElementGroup())
      case _ if isDefined(ctx.multiElementOneOf()) => visitMultiElementOneOf(ctx.multiElementOneOf())
      case _ => err("visitInnerShape. Unknown alternative")
    }

  override def visitSingleElementGroup(
    ctx: SingleElementGroupContext): Builder[TripleExpr] = {
    visitUnaryTripleExpr(ctx.unaryTripleExpr())
  }

  override def visitMultiElementGroup(
    ctx: MultiElementGroupContext): Builder[TripleExpr] =
    for {
      ses <- visitList(visitUnaryTripleExpr, ctx.unaryTripleExpr())
    } yield ses.length match {
      case 1 => ses.head
      case _ => EachOf(None, expressions = ses, None, None, None, None)
    }

  override def visitMultiElementOneOf(ctx: MultiElementOneOfContext): Builder[TripleExpr] = for {
    groups <- visitList(visitGroupTripleExpr, ctx.groupTripleExpr)
  } yield groups.length match {
    case 1 => groups.head
    case _ => OneOf(None, expressions = groups, None, None, None, None)
  }

  override def visitShapeExprLabel(ctx: ShapeExprLabelContext): Builder[ShapeLabel] = {
    ctx match {
      case _ if (isDefined(ctx.iri())) => {
        for {
          iri <- visitIri(ctx.iri())
        } yield IRILabel(iri)
      }
      case _ if (isDefined(ctx.blankNode())) => {
        for {
          bNode <- visitBlankNode(ctx.blankNode())
        } yield BNodeLabel(bNode)
      }
      case _ => err("visitShapeExprLabel, no IRI and no BNode")
    }
  }

  override def visitBlankNode(ctx: BlankNodeContext): Builder[BNodeId] = {
    ok(BNodeId(removeUnderscore(ctx.BLANK_NODE_LABEL().getText())))
  }

  def removeUnderscore(str: String): String =
    str.drop(2)

  def getPrefixes(ds: List[Directive]): Map[Prefix, IRI] = {
    def comb(rest: Map[Prefix, IRI], x: Directive): Map[Prefix, IRI] = {
      x.fold(p => rest + p, _ => rest)
    }

    def zero: Map[Prefix, IRI] = Map()

    ds.foldLeft(zero)(comb)
  }

  override def visitDirective(
    ctx: DirectiveContext): Builder[Directive] = ctx match {
    case _ if (isDefined(ctx.baseDecl())) => for {
        iri <- visitBaseDecl(ctx.baseDecl())
      } yield Right(Left(iri))
    case _ if (isDefined(ctx.prefixDecl())) => for {
        p <- visitPrefixDecl(ctx.prefixDecl())
      } yield Left(p)
    case _ if (isDefined(ctx.importDecl())) => for {
      iri <- visitImportDecl(ctx.importDecl())
    } yield Right(Right(iri))
    case _ => err(s"visitDirective: unknown directive")
  }


  override def visitImportDecl(ctx: ImportDeclContext): Builder[IRI] = for {
    iri <- visitIri(ctx.iri())
  } yield iri

  // TODO: Resolve base taking into account previous base declarations ?
  override def visitBaseDecl(ctx: BaseDeclContext): Builder[IRI] = {
    val baseIri = extractIRIfromIRIREF(ctx.IRIREF().getText, None)
    for {
      _ <- addBase(baseIri)
    } yield baseIri
  }

  override def visitPrefixDecl(ctx: PrefixDeclContext): Builder[(Prefix, IRI)] = {
    val prefix = Prefix(ctx.PNAME_NS().getText.init)
    // TODO Resolve prefix declarations taking into account base?
    val iri = extractIRIfromIRIREF(ctx.IRIREF().getText, None)
    for {
      _ <- addPrefix(prefix, iri)
    } yield (prefix, iri)
  }

  sealed trait Qualifier {
    def getExtras: List[IRI] = {
      this match {
        case Extra(iris) => iris
        case _ => List()
      }
    }
    def getIncluded: List[ShapeLabel] = {
      this match {
        case Include(labels) => labels
        case _ => List()
      }
    }
  }

  case class Extra(iris: List[IRI]) extends Qualifier

  case class Include(labels: List[ShapeLabel]) extends Qualifier

  case object Closed extends Qualifier

  // Some generic utils

  def isDefined[A](x: A): Boolean = x != null

  def visitList[A, B](
    visitFn: A => Builder[B],
    ls: java.util.List[A]): Builder[List[B]] =
    ls.asScala.toList.map(visitFn(_)).sequence

  def visitOpt[A, B](
    visitFn: A => Builder[B],
    v: A): Builder[Option[B]] =
    if (isDefined(v)) visitFn(v).map(Some(_))
    else ok(None)

  /* Remove @ from language tag */
  private def getLanguage(str: String): Lang =
    Lang(str.tail)

}
