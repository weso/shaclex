package es.weso.shex.compact
import java.util
import cats.implicits._
import es.weso.rdf.Prefix
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.shex._
import es.weso.shex.parser.ShExDocBaseVisitor
import es.weso.shex.compact.Parser._
import org.antlr.v4.runtime._
// import org.antlr.v4.runtime.atn.ATNConfigSet
// import org.antlr.v4.runtime.dfa.DFA
import es.weso.shex.parser.ShExDocParser.{StringContext => ShExStringContext, _}
import es.weso.shex.parser._
import scala.collection.JavaConverters._

/**
  * Visits the AST and builds the corresponding ShEx abstract syntax
  */
class SchemaMaker extends ShExDocBaseVisitor[Any] {

  type Start = Option[ShapeExpr]
  type NotStartAction = Either[Start, (ShapeLabel, ShapeExpr)]
  type Cardinality = (Option[Int], Option[Max])
  type Directive = Either[(Prefix, IRI), IRI]

  val star = (Some(0), Some(Star))
  val plus = (Some(1), Some(Star))
  val optional = (Some(0), Some(IntMax(1)))

  override def visitShExDoc(
                             ctx: ShExDocContext
                           ): Builder[Schema] = {
    for {
      directives <- visitList(visitDirective, ctx.directive())
      startActions <- visitStartActions(ctx.startActions())
      notStartAction <- visitNotStartAction(ctx.notStartAction())
      statements <- visitList(visitStatement, ctx.statement())

      prefixMap <- getPrefixMap
      base <- getBase
      start <- getStart
      shapesMap <- getShapesMap
    } yield {
      Schema.empty.copy(
        prefixes = if (!prefixMap.isEmpty) Some(prefixMap) else None,
        base = base,
        startActs = startActions,
        start = start,
        shapes = if (!shapesMap.isEmpty) Some(shapesMap) else None
      )
    }
  }

  override def visitStatement(
                               ctx: StatementContext
                             ): Builder[Unit] = ctx match {
    case _ if (isDefined(ctx.directive())) =>
      visitDirective(ctx.directive()).map(_ => ())
    case _ if (isDefined(ctx.notStartAction())) =>
      visitNotStartAction(ctx.notStartAction()).map(_ => ())
  }

  override def visitNotStartAction(
                                    ctx: NotStartActionContext
                                  ): Builder[NotStartAction] = if (ctx == null) ok(Left(None))
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
                           ctx: StartContext):
  Builder[Option[(ShapeExpr)]] = {
    if (isDefined(ctx)) {
      for {
        shapeExpr <- visitShapeExpression(ctx.shapeExpression())
      // semActs <- visitSemanticActions(ctx.semanticActions())
      } yield Some((shapeExpr))
    } else
      ok(None)
  }

  override def visitSemanticActions(ctx: SemanticActionsContext): Builder[List[SemAct]] = {
    val r: List[Builder[SemAct]] =
      ctx.codeDecl().asScala.map(visitCodeDecl(_)).toList
    r.sequence
  }

  override def visitCodeDecl(
                              ctx: CodeDeclContext): Builder[SemAct] =
    for {
      iri <- visitIri(ctx.iri())
      code <- optBuilder(ctx.CODE()).map(opt => opt.map(_.getText()))
      str <- optMapBuilder(code, cleanCode)
    } yield SemAct(iri, str)

  def cleanCode(str: String): Builder[String] = {
    val codeRegex = "^\\{(.*)%\\}$".r
    str match {
      case codeRegex(c) => ok(c)
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
                           f: A => Builder[B]
                         ): Builder[Option[B]] =
    x match {
      case None => ok(None)
      case Some(v) => f(v).map(Some(_))
    }

  override def visitShapeExprDecl(ctx: ShapeExprDeclContext): Builder[(ShapeLabel, ShapeExpr)] =
    for {
      label <- visitShapeLabel(ctx.shapeLabel())
      shapeExpr <- obtainShapeExpr(ctx)
      _ <- addShape(label, shapeExpr)
    } yield (label, shapeExpr)

  def obtainShapeExpr(ctx: ShapeExprDeclContext): Builder[ShapeExpr] =
    if (isDefined(ctx.KW_EXTERNAL())) {
      // TODO: What happens if there are semantic actions after External??
      ok(ShapeExternal())
    } else
    // TODO: Obtain stringFacet*
      visitShapeExpression(ctx.shapeExpression())

  override def visitShapeExpression(ctx: ShapeExpressionContext): Builder[ShapeExpr] =
    visitShapeOr(ctx.shapeOr())

  override def visitInlineShapeExpression(
                                           ctx: InlineShapeExpressionContext): Builder[ShapeExpr] =
    visitInlineShapeOr(ctx.inlineShapeOr())

  override def visitInlineShapeOr(
                                   ctx: InlineShapeOrContext):
  Builder[ShapeExpr] = for {
    shapes <- {
      val r: List[Builder[ShapeExpr]] =
        ctx.inlineShapeAnd().asScala.map(visitInlineShapeAnd(_)).toList
      r.sequence
    }
  } yield if (shapes.length == 1) shapes.head
  else ShapeOr(shapes)

  override def visitInlineShapeAnd(
                                    ctx: InlineShapeAndContext):
  Builder[ShapeExpr] = {
    for {
      shapes <- {
        val r: List[Builder[ShapeExpr]] =
          ctx.inlineShapeNot().asScala.map(visitInlineShapeNot(_)).toList
        r.sequence
      }
    } yield if (shapes.length == 1) shapes.head
    else ShapeAnd(shapes)
  }

  override def visitInlineShapeNot(
                                    ctx: InlineShapeNotContext):
  Builder[ShapeExpr] = for {
    shapeAtom <- visitInlineShapeAtom(ctx.inlineShapeAtom())
  } yield if (isDefined(ctx.negation()))
    ShapeNot(shapeAtom)
  else shapeAtom

  override def visitShapeOr(ctx: ShapeOrContext):
  Builder[ShapeExpr] = for {
    shapes <- {
      val r: List[Builder[ShapeExpr]] =
        ctx.shapeAnd().asScala.map(visitShapeAnd(_)).toList
      r.sequence
    }
  } yield if (shapes.length == 1) shapes.head
  else ShapeOr(shapes)

  override def visitShapeAnd(
                              ctx: ShapeAndContext):
  Builder[ShapeExpr] = {
    for {
      shapes <- {
        val r: List[Builder[ShapeExpr]] =
          ctx.shapeNot().asScala.map(visitShapeNot(_)).toList
        r.sequence
      }
    } yield if (shapes.length == 1) shapes.head
    else ShapeAnd(shapes)
  }

  override def visitShapeNot(
                              ctx: ShapeNotContext):
  Builder[ShapeExpr] = for {
    shapeAtom <- visitShapeAtom(ctx.shapeAtom())
  } yield if (isDefined(ctx.negation()))
    ShapeNot(shapeAtom)
  else shapeAtom

  def visitShapeAtom(ctx: ShapeAtomContext):
  Builder[ShapeExpr] = {
    ctx match {
      case s: ShapeAtomNodeConstraintContext =>
        for {
          nk <- visitNodeConstraint(s.nodeConstraint())
        // TODO: Check shapeOrRef?
        } yield nk

      case s: ShapeAtomShapeOrRefContext =>
        visitShapeOrRef(s.shapeOrRef())

      case s: ShapeAtomShapeExpressionContext =>
        visitShapeExpression(s.shapeExpression())

      case s: ShapeAtomAnyContext =>
        ok(ShapeExpr.any)

      case _ => err(s"Internal error visitShapeAtom: unknown ctx $ctx")
    }
  }

  def visitNodeConstraint(ctx: NodeConstraintContext): Builder[NodeConstraint] = {
    ctx match {
      case s: NodeConstraintLiteralContext =>
        for {
          xsFacets <- visitList(visitXsFacet, s.xsFacet())
        } yield NodeConstraint.nodeKind(LiteralKind).copy(
          xsFacets = xsFacets
        )
      case s: NodeConstraintNonLiteralContext =>
        for {
          nodeKind <- visitNonLiteralKind(s.nonLiteralKind())
          stringFacets <- visitList(visitStringFacet, s.stringFacet())
        } yield NodeConstraint.nodeKind(nodeKind).copy(
          xsFacets = stringFacets
        ) // Todo add "shapeOrRef?"
      case s: NodeConstraintDatatypeContext =>
        for {
          datatype <- visitDatatype(s.datatype())
          xsFacets <- visitList(visitXsFacet, s.xsFacet())
        } yield NodeConstraint.datatype(datatype).copy(
          xsFacets = xsFacets
        )
      case c: NodeConstraintValueSetContext =>
        for {
         vs <- visitValueSet(c.valueSet())
         xsFacets <- visitList(visitXsFacet, c.xsFacet())
        } yield NodeConstraint.valueSet(vs).copy(
            xsFacets = xsFacets
        )

      case c: NodeConstraintFacetContext =>
        err("NodeConstraintFacetContext")
    }
  }

  def visitInlineShapeAtom(ctx: InlineShapeAtomContext): Builder[ShapeExpr] = {
    ctx match {
      case s: InlineShapeAtomNodeConstraintContext =>
        for {
          nk <- visitNodeConstraint(s.nodeConstraint())
        // TODO: Check shapeOrRef?
        } yield nk

      case s: InlineShapeAtomShapeOrRefContext =>
        visitInlineShapeOrRef(s.inlineShapeOrRef())

      case s: InlineShapeAtomShapeExpressionContext =>
        visitShapeExpression(s.shapeExpression())

      case s: InlineShapeAtomAnyContext =>
        ok(ShapeExpr.any)
    }
  }

  override def visitValueSet(
                              ctx: ValueSetContext): Builder[List[ValueSetValue]] = {
    visitList(visitValueSetValue, ctx.valueSetValue())
  }

  override def visitValueSetValue(
                                   ctx: ValueSetValueContext): Builder[ValueSetValue] = {
    if (isDefined(ctx.iriRange()))
      visitIriRange(ctx.iriRange())
    else
      visitLiteral(ctx.literal())
  }

  override def visitIriRange(
                              ctx: IriRangeContext): Builder[ValueSetValue] = {
    throw new Exception("Visit IRI range")
  }

  override def visitLiteral(
                             ctx: LiteralContext): Builder[ValueSetValue] = {
    if (isDefined(ctx.rdfLiteral()))
      visitRdfLiteral(ctx.rdfLiteral())
    else if (isDefined(ctx.numericLiteral()))
      visitNumericLiteral(ctx.numericLiteral())
    else if (isDefined(ctx.booleanLiteral()))
      visitBooleanLiteral(ctx.booleanLiteral())
    else err(s"visitLiteral: Unknown ${ctx}")
  }

  override def visitRdfLiteral(
                                ctx: RdfLiteralContext): Builder[ValueSetValue] = {
    val str = visitString(ctx.string())
    if (isDefined(ctx.LANGTAG())) {
      val lang = ctx.LANGTAG().getText()
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

  override def visitString(
                            ctx: ShExStringContext): Builder[String] = {
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

  override def visitIri(ctx: IriContext): Builder[IRI] = {
    if (isDefined(ctx.IRIREF())) {
      ok(extractIRIfromIRIREF(ctx.IRIREF().getText))
    } else {
      val prefixedName = visitPrefixedName(ctx.prefixedName())
      resolve(prefixedName)
    }
  }

  def resolve(prefixedName: String): Builder[IRI] = {
    val (prefix, local) = splitPrefix(prefixedName)
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
      (prefix + ":", name.tail)
    } else {
      ("", str)
    }
  }

  override def visitPrefixedName(ctx: PrefixedNameContext): String =
    ctx match {
      case _ if (isDefined(ctx.PNAME_LN())) => ctx.PNAME_LN().getText()
      case _ if (isDefined(ctx.PNAME_LN())) => ctx.PNAME_LN().getText()
    }

  /*   override def visitNodeConstraintGroup(ctx: NodeConstraintGroupContext): Builder[ShapeExpr] =
       for {
        shapeOrRef <- visitShapeOrRef(ctx.shapeOrRef())
      } yield shapeOrRef */

  def extractIRIfromIRIREF(d: String): IRI = {
    val iriRef = "^<(.*)>$".r
    d match {
      case iriRef(i) => IRI(i)
    }
  }

  override def visitNumericLiteral(
                                    ctx: NumericLiteralContext): Builder[ValueSetValue] = {
    if (isDefined(ctx.INTEGER())) {
      getInteger(ctx.INTEGER().getText()).map(ObjectValue.intValue(_))
    }
    else if (isDefined(ctx.DECIMAL())) {
      getDecimal(ctx.DECIMAL().getText()).map(ObjectValue.decimalValue(_))
    }
    else if (isDefined(ctx.DOUBLE())) {
      getDouble(ctx.DOUBLE().getText()).map(ObjectValue.doubleValue(_))
    } else err(s"visitNumericLiteral: Unknown $ctx")
  }

  def getInteger(str: String): Builder[Int] = {
    try {
      ok(str.toInt)
    } catch {
      case e: NumberFormatException =>
        err(s"Cannot get integer from $str")
    }
  }

  def getDecimal(str: String): Builder[BigDecimal] = {
    try {
      ok(BigDecimal(str))
    } catch {
      case e: NumberFormatException =>
        err(s"Cannot get decimal from $str")
    }
  }

  def getDouble(str: String): Builder[Double] = {
    try {
      ok(str.toDouble)
    } catch {
      case e: NumberFormatException =>
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

  def isDefined[A](x: A): Boolean =
    if (x != null) true
    else false

  def visitList[A, B](
                       visitFn: A => Builder[B],
                       ls: java.util.List[A]): Builder[List[B]] =
    ls.asScala.toList.map(visitFn(_)).sequence

  override def visitXsFacet(ctx: XsFacetContext): Builder[XsFacet] =
    ctx match {
      case _ if (isDefined(ctx.stringFacet())) =>
        visitStringFacet(ctx.stringFacet())
      case _ if (isDefined(ctx.numericFacet())) =>
        visitNumericFacet(ctx.numericFacet())
      case _ => err(s"visitXsFacet: Unsupported ${ctx.getClass.getName}")
    }

  override def visitStringFacet(
                                 ctx: StringFacetContext): Builder[XsFacet] = {
    if (isDefined(ctx.stringLength())) {
      for {
        n <- getInteger(ctx.INTEGER().getText())
        stringLength <- visitStringLength(ctx.stringLength)(n)
      } yield stringLength
    } else {
      // pattern
      for {
        str <- visitString(ctx.string())
      } yield Pattern(str)
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

  override def visitNumericFacet(
                                  ctx: NumericFacetContext): Builder[XsFacet] = {
    err("not implemented numeric facet yet")
  }

  override def visitNodeConstraintLiteral(ctx: NodeConstraintLiteralContext): Builder[ShapeExpr] = {
    ok(NodeConstraint.nodeKind(LiteralKind)) // TODO: xsFacet*
  }

  override def visitNodeConstraintNonLiteral(ctx: NodeConstraintNonLiteralContext): Builder[ShapeExpr] = {
    visitNonLiteralKind(ctx.nonLiteralKind()).map(nk => NodeConstraint.nodeKind(nk)) // TODO: xsFacet*
  }

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
    case _ if (isDefined(ctx.shapeLabel())) =>
      visitShapeLabel(ctx.shapeLabel()).map(ShapeRef(_))
    case _ if (isDefined(ctx.ATPNAME_NS())) => {
      val nameNS = ctx.ATPNAME_NS().getText().tail
      resolve(nameNS).map(iri => ShapeRef(IRILabel(iri)))
    }
    case _ if (isDefined(ctx.ATPNAME_LN())) => {
      val nameLN = ctx.ATPNAME_LN().getText().tail
      resolve(nameLN).map(iri => ShapeRef(IRILabel(iri)))
    }
    case _ => err(s"internal Error: visitShapeOrRef. Unknown $ctx")
  }

  override def visitShapeOrRef(ctx: ShapeOrRefContext): Builder[ShapeExpr] = ctx match {
    case _ if (isDefined(ctx.shapeDefinition())) =>
      visitShapeDefinition(ctx.shapeDefinition())
    case _ if (isDefined(ctx.shapeLabel())) =>
      visitShapeLabel(ctx.shapeLabel()).map(ShapeRef(_))
    case _ if (isDefined(ctx.ATPNAME_NS())) => {
      val nameNS = ctx.ATPNAME_NS().getText().tail
      resolve(nameNS).map(iri => ShapeRef(IRILabel(iri)))
    }
    case _ if (isDefined(ctx.ATPNAME_LN())) => {
      val nameLN = ctx.ATPNAME_LN().getText().tail
      resolve(nameLN).map(iri => ShapeRef(IRILabel(iri)))
    }
    case _ => err(s"internal Error: visitShapeOrRef. Unknown $ctx")
  }

  override def visitInlineShapeDefinition(ctx: InlineShapeDefinitionContext): Builder[ShapeExpr] = {
    for {
      qualifiers <- visitList(visitQualifier, ctx.qualifier())
      tripleExpr <- visitSomeOfShape(ctx.someOfShape())
    } yield makeShape(qualifiers,tripleExpr)
  }

  override def visitShapeDefinition(ctx: ShapeDefinitionContext): Builder[ShapeExpr] = {
    for {
      qualifiers <- visitList(visitQualifier, ctx.qualifier())
      tripleExpr <- visitSomeOfShape(ctx.someOfShape())
    } yield makeShape(qualifiers,tripleExpr)
  }

  def makeShape(qualifiers: List[Qualifier], tripleExpr: Option[TripleExpr]): ShapeExpr = {
    val containsClosed =
      if (qualifiers.contains(Closed))
        Some(true)
      else
        Some(false)
    val ls: List[IRI] = qualifiers.map(_.getExtras).flatten
    val extras: Option[List[IRI]] =
      if (ls.isEmpty) None
      else Some(ls)
    // TODO: Rest of qualifiers Virtual, inherit
    Shape.empty.copy(
      closed = containsClosed,
      extra = extras,
      expression = tripleExpr
    )
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

  override def visitIncludeSet(ctx: IncludeSetContext): Builder[Qualifier] = {
    throw new Exception("not implemented IncludeSet yet")
  }

  override def visitExtraPropertySet(ctx: ExtraPropertySetContext): Builder[Qualifier] = for {
    ls <- visitList(visitPredicate, ctx.predicate())
  } yield Extra(ls)

  override def visitSomeOfShape(
                                 ctx: SomeOfShapeContext): Builder[Option[TripleExpr]] = {
    if (isDefined(ctx)) {
      ctx match {
        case _ if (isDefined(ctx.groupShape())) => for {
          tripleExpr <- visitGroupShape(ctx.groupShape())
        } yield Some(tripleExpr)
        case _ if (isDefined(ctx.multiElementSomeOf())) => for {
          tripleExpr <- visitMultiElementSomeOf(ctx.multiElementSomeOf())
        } yield Some(tripleExpr)
        case _ => err(s"visitSomeOfShape: unknown $ctx")
      }
    } else ok(None)
  }

  override def visitGroupShape(
                                ctx: GroupShapeContext): Builder[TripleExpr] = {
    ctx match {
      case _ if (isDefined(ctx.singleElementGroup())) =>
        visitSingleElementGroup(ctx.singleElementGroup())
      case _ if (isDefined(ctx.multiElementGroup())) =>
        visitMultiElementGroup(ctx.multiElementGroup())
      case _ => err(s"visitGroupShape: unknown $ctx")
    }
  }

  override def visitUnaryShape(
                                ctx: UnaryShapeContext): Builder[TripleExpr] = {
    ctx match {
      case _ if (isDefined(ctx.tripleConstraint())) =>
        // TODO: productionLabel?
        visitTripleConstraint(ctx.tripleConstraint())
      case _ if (isDefined(ctx.include())) =>
        visitInclude(ctx.include())
      case _ if (isDefined(ctx.encapsulatedShape())) =>
        visitEncapsulatedShape(ctx.encapsulatedShape())
      case _ => err(s"visitGroupShape: unknown $ctx")
    }
  }

  override def visitTripleConstraint(
                                      ctx: TripleConstraintContext): Builder[TripleExpr] =
    for {
      predicate <- visitPredicate(ctx.predicate())
      shapeExpr <- visitInlineShapeExpression(ctx.inlineShapeExpression())
      cardinality <- getCardinality(ctx.cardinality())
    } yield
      TripleConstraint.
        emptyPred(predicate).copy(
        valueExpr = Some(shapeExpr),
        optMin = cardinality._1,
        optMax = cardinality._2
      )

  def getCardinality(
                      ctx: CardinalityContext): Builder[Cardinality] = {
    if (isDefined(ctx))
      ctx match {
        case s: StarCardinalityContext => ok(star)
        case s: PlusCardinalityContext => ok(plus)
        case s: OptionalCardinalityContext => ok(optional)
        case s: RepeatCardinalityContext => visitRepeatCardinality(s)
        case _ => err(s"Not implemented cardinality ${ctx.getClass.getName}")
      } else ok((None, None))
  }

  override def visitRepeatCardinality(
                                       ctx: RepeatCardinalityContext): Builder[Cardinality] = {
    visitRepeatRange(ctx.repeatRange())
  }

  override def visitRepeatRange(
                                 ctx: RepeatRangeContext): Builder[Cardinality] = {
    for {
      min <- visitMin_range(ctx.min_range())
      max <- visitMax_range(ctx.max_range())
    } yield (min, max)
  }

  override def visitMin_range(
                               ctx: Min_rangeContext): Builder[Option[Int]] = {
    if (isDefined(ctx)) {
      getInteger(ctx.INTEGER().getText()).map(Some(_))
    } else ok(None)
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
                             ctx: IncludeContext): Builder[TripleExpr] = {
    err("Not implemented include yet")
  }

  override def visitEncapsulatedShape(
                                       ctx: EncapsulatedShapeContext): Builder[TripleExpr] = {
    err("Not implemented encapsulatedShape yet")
  }

  override def visitSingleElementGroup(
                                        ctx: SingleElementGroupContext): Builder[TripleExpr] = {
    visitUnaryShape(ctx.unaryShape())
  }

  override def visitMultiElementGroup(
                                       ctx: MultiElementGroupContext): Builder[TripleExpr] = for {
    ses <- visitList(visitUnaryShape, ctx.unaryShape())
  } yield EachOf(ses, None, None, None, None)

  override def visitMultiElementSomeOf(
                                        ctx: MultiElementSomeOfContext): Builder[TripleExpr] = {
    throw new Exception("visitMultiElementSomeOf")
  }

  override def visitShapeLabel(ctx: ShapeLabelContext): Builder[ShapeLabel] = {
    if (isDefined(ctx.iri())) {
      for {
        iri <- visitIri(ctx.iri())
      } yield IRILabel(iri)
    } else {
      throw new Exception(s"visitShapeLabel $ctx")
    }
  }

  override def visitBlankNode(ctx: BlankNodeContext): BNodeId = {
    throw new Exception("visitBlankNode")
  }

  def getPrefixes(ds: List[Directive]): Map[Prefix, IRI] = {
    def comb(rest: Map[Prefix, IRI], x: Directive): Map[Prefix, IRI] = {
      x.fold(p => rest + p, _ => rest)
    }

    def zero: Map[Prefix, IRI] = Map()

    ds.foldLeft(zero)(comb)
  }

  override def visitDirective(
                               ctx: DirectiveContext): Builder[Directive] = {
    if (ctx.baseDecl() != null) {
      for {
        iri <- visitBaseDecl(ctx.baseDecl())
      } yield Right(iri)
    } else {
      for {
        p <- visitPrefixDecl(ctx.prefixDecl())
      } yield Left(p)
    }
  }

  override def visitBaseDecl(ctx: BaseDeclContext): Builder[IRI] = {
    val baseIri = extractIRIfromIRIREF(ctx.IRIREF().getText)
    for {
      _ <- addBase(baseIri)
    } yield baseIri
  }

  /*   def getBase(ds: List[Directive]): Option[IRI] = {
       def comb(rest: Option[IRI],x: Directive): Option[IRI] = {
         x.fold(_ => rest, iri => combineBase(rest,iri))
       }
       def zero: Option[IRI] = None
       ds.foldLeft(zero)(comb)
     } */

  /*   def combineBase(rest: Option[IRI], iri: IRI): Option[IRI] = {
       rest match {
         case None => Some(iri)
         case Some(i) => Some(iri) // Combine if iri is a relative IRI?
       }
     } */

  override def visitPrefixDecl(ctx: PrefixDeclContext): Builder[(Prefix, IRI)] = {
    val prefix = Prefix(ctx.PNAME_NS().getText)
    val iri = extractIRIfromIRIREF(ctx.IRIREF().getText)
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
  }

  case class Extra(iris: List[IRI]) extends Qualifier

  case class Include(label: ShapeLabel) extends Qualifier

  case object Closed extends Qualifier

}
