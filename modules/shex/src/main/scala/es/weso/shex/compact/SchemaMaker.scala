package es.weso.shex.compact
import java.util

import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.Prefix
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.shex._
import es.weso.shex.parser.ShExDocBaseVisitor
import es.weso.shex.compact.Parser._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.ParseTree
// import org.antlr.v4.runtime.atn.ATNConfigSet
// import org.antlr.v4.runtime.dfa.DFA
import es.weso.shex.parser.ShExDocParser.{StringContext => ShExStringContext, _}
import es.weso.shex.parser._
import scala.collection.JavaConverters._

/**
  * Visits the AST and builds the corresponding ShEx abstract syntax
  */
class SchemaMaker extends ShExDocBaseVisitor[Any] with LazyLogging {

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
      shapeMap <- getShapesMap
    } yield {
      Schema.empty.copy(
        prefixes = if (!prefixMap.isEmpty) Some(prefixMap) else None,
        base = base,
        startActs = startActions,
        start = start,
        shapes = if (!shapeMap.isEmpty) Some(shapesMap2List(shapeMap)) else None
      )
    }
  }

  def shapesMap2List(sm: ShapesMap): List[ShapeExpr] = {
    sm.map{case (lbl,se) => se.addId(lbl)}.toList
  }

  override def visitStatement(
                               ctx: StatementContext
                             ): Builder[Unit] = ctx match {
    case _ if (isDefined(ctx.directive())) =>
      visitDirective(ctx.directive()).map(_ => ())
    case _ if (isDefined(ctx.notStartAction())) =>
      visitNotStartAction(ctx.notStartAction()).map(_ => ())
  }

  override def
    visitNotStartAction(ctx: NotStartActionContext
      ): Builder[NotStartAction] =
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
                           ctx: StartContext):
  Builder[Option[(ShapeExpr)]] = {
    logger.info(s"Visiting start...$ctx")
    if (isDefined(ctx)) {
      logger.info(s"is defined...")
      for {
        shapeExpr <- visitShapeExpression(ctx.shapeExpression())
        _ <- {
          logger.info(s"Shape expression for start: $shapeExpr")
          updateStart(Some(shapeExpr))
        }
      // semActs <- visitSemanticActions(ctx.semanticActions())
      } yield {
        logger.info(s"Shape expression for start: $shapeExpr")
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
                                   ctx: InlineShapeOrContext):
  Builder[ShapeExpr] = for {
    shapes <- {
      val r: List[Builder[ShapeExpr]] =
        ctx.inlineShapeAnd().asScala.map(visitInlineShapeAnd(_)).toList
      r.sequence
    }
  } yield if (shapes.length == 1) shapes.head
  else ShapeOr(None, shapes)

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
    else ShapeAnd(None,shapes)
  }

  override def visitInlineShapeNot(
                                    ctx: InlineShapeNotContext):
  Builder[ShapeExpr] = for {
    shapeAtom <- visitInlineShapeAtom(ctx.inlineShapeAtom())
  } yield
    if (isDefined(ctx.negation()))
      ShapeNot(None,shapeAtom)
    else
      shapeAtom

  override def visitShapeOr(ctx: ShapeOrContext):
  Builder[ShapeExpr] = for {
    shapes <- {
      val r: List[Builder[ShapeExpr]] =
        ctx.shapeAnd().asScala.map(visitShapeAnd(_)).toList
      r.sequence
    }
  } yield if (shapes.length == 1) shapes.head
  else ShapeOr(None,shapes)

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
    else ShapeAnd(None,shapes)
  }

  override def visitShapeNot(
                              ctx: ShapeNotContext):
  Builder[ShapeExpr] = for {
    shapeAtom <- visitShapeAtom(ctx.shapeAtom())
  } yield if (isDefined(ctx.negation()))
    ShapeNot(None,shapeAtom)
  else shapeAtom

  def visitShapeAtom(ctx: ShapeAtomContext):
  Builder[ShapeExpr] = {
    ctx match {
      case s: ShapeAtomNodeConstraintContext =>
        for {
          nk <- visitNodeConstraint(s.nodeConstraint())
          sr <- visitOpt(visitShapeOrRef, s.shapeOrRef())
        } yield sr match {
          case None => nk
          case Some(s) => ShapeAnd(None,List(nk,s))
        }

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
        } yield NodeConstraint.nodeKind(LiteralKind,xsFacets)
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
        xsFacets = facets
      )
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
          case Some(s) => ShapeAnd(None, List(nk,s))
        }

      case s: InlineShapeAtomShapeOrRefContext =>
        for {
         sr <- visitInlineShapeOrRef(s.inlineShapeOrRef())
          nk <- visitOpt(visitNodeConstraint,s.nodeConstraint())
        } yield nk match {
          case None => sr
          case Some(n) => ShapeAnd(None,List(sr,n))
        }

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
    ctx match {
      case _ if isDefined(ctx.iri()) => // Rule: iri ('~' exclusion*)?
        if (ctx.getChildCount == 1)   // Only iri
          for {
          iri <- visitIri(ctx.iri())
        } yield IRIValue(iri)
        else for {  // iri '~' exclusion*
        iri <- visitIri(ctx.iri())
        exclusions <-
           if (isDefined(ctx.exclusion()))
            visitList(visitExclusion,ctx.exclusion()).map(Some(_))
           else ok(None)
      } yield StemRange(IRIStem(iri), exclusions)
      case _ => for {
        exclusions <- visitList(visitExclusion, ctx.exclusion())
      } yield StemRange(Wildcard(),Some(exclusions))
    }
  }

  override def visitExclusion(ctx: ExclusionContext): Builder[ValueSetValue] = for {
    iri <- visitIri(ctx.iri())
  } yield {
    // TODO: Detect if the rule has a ~ at the end:
    //   exclusion       : '-' iri '~'? ;
    println(s"Exclusion count: ${ctx.getChildCount}")
    IRIValue(iri)
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

  private def numericLiteral2ValueObject(nl: NumericLiteral): Builder[ValueSetValue] = {
    nl match {
      case NumericInt(n) => ok(ObjectValue.intValue(n))
      case NumericDouble(d) => ok(ObjectValue.doubleValue(d))
      case NumericDecimal(d) => ok(ObjectValue.decimalValue(d))
    }
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

  override def visitIri(ctx: IriContext): Builder[IRI] =
    if (isDefined(ctx.IRIREF())) for {
      base <- getBase
    } yield
       extractIRIfromIRIREF(ctx.IRIREF().getText, base)
   else for {
    prefixedName <- visitPrefixedName(ctx.prefixedName())
    iri <- resolve(prefixedName)
  } yield iri

  def resolve(prefixedName: String): Builder[IRI] = {
    val (prefix, local) = splitPrefix(prefixedName)
    logger.info(s"Resolve. prefix: $prefix local: $local Prefixed name: $prefixedName")
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
    val iriRef = "^<(.*)>$".r
    d match {
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
      case _ if (isDefined(ctx.DECIMAL())) =>
        ok(NumericDecimal(BigDecimal(ctx.DECIMAL().getText)))
      case _ if (isDefined(ctx.DOUBLE())) => {
        val str = ctx.DOUBLE().getText
        ok(NumericDecimal(str.toDouble))
      }
      case _ => err("Unknown ctx in numericLiteral")
    }
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
      // TODO. Update pattern to handle flags
      for {
        str <- visitString(ctx.string())
      } yield Pattern(str,None)
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
        v <- if (isDefined(ctx.numericLiteral())) {
               visitNumericLiteral(ctx.numericLiteral())
             } else for {
               lexicalForm <- visitString(ctx.string())
               datatype <- visitDatatype(ctx.datatype())
               numericLiteral <- makeNumericLiteral(lexicalForm,datatype)
             } yield numericLiteral
        nf <- makeNumericFacet(nr, v)
      } yield nf
      case _ if isDefined(ctx.numericLength()) => for {
        nl <- visitNumericLength(ctx.numericLength())
        n <- getInteger(ctx.INTEGER().getText)
        numLength <- makeNumericLength(nl,n)
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
      } yield NumericDecimal(d)
      case `xsd_double` => for {
        d <- getDouble(lexicalForm)
      } yield NumericDouble(d)
      case `xsd_float` => for {  // TODO: Check if floats and doubles are equivalent
        d <- getDouble(lexicalForm)
      } yield NumericDouble(d)
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
  override def
    visitNodeConstraintLiteral(ctx: NodeConstraintLiteralContext): Builder[ShapeExpr] = for {
    facets <- visitList(visitXsFacet, ctx.xsFacet())
  } yield NodeConstraint.nodeKind(LiteralKind, facets)

  override def
     visitNodeConstraintNonLiteral(ctx: NodeConstraintNonLiteralContext): Builder[ShapeExpr] = for {
    nk <- visitNonLiteralKind(ctx.nonLiteralKind())
    facets <- visitList(visitStringFacet,ctx.stringFacet())
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
      tripleExpr <- visitOneOfShape(ctx.oneOfShape())
      shape <- makeShape(qualifiers,tripleExpr,List())
    } yield shape
  }

  override def visitShapeDefinition(ctx: ShapeDefinitionContext): Builder[ShapeExpr] = {
    for {
      qualifiers <- visitList(visitQualifier, ctx.qualifier())
      tripleExpr <- visitOneOfShape(ctx.oneOfShape())
      semActs <- visitSemanticActions(ctx.semanticActions())
      anns <- visitList(visitAnnotation,ctx.annotation())
      // newTripleExpr <- addAnnotations(tripleExpr,anns)
      shape <- makeShape(qualifiers,tripleExpr,semActs)
    } yield shape
  }

  // TODO: Maybe remove the following method
  def addAnnotations(maybeTe: Option[TripleExpr],
                     anns: List[Annotation]):
      Builder[Option[TripleExpr]] =
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
                 semActs: List[SemAct]): Builder[ShapeExpr] = {
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
      semActs = if (semActs.isEmpty) None else Some(semActs)
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

  override def
    visitIncludeSet(ctx: IncludeSetContext):
       Builder[Qualifier] = for {
    sl <- visitList(visitShapeLabel,ctx.shapeLabel())
  } yield Include(sl)

  override def visitExtraPropertySet(ctx: ExtraPropertySetContext): Builder[Qualifier] = for {
    ls <- visitList(visitPredicate, ctx.predicate())
  } yield Extra(ls)

  override def visitOneOfShape(
                                 ctx: OneOfShapeContext): Builder[Option[TripleExpr]] = {
    if (isDefined(ctx)) {
      ctx match {
        case _ if (isDefined(ctx.groupShape())) => for {
          tripleExpr <- visitGroupShape(ctx.groupShape())
        } yield Some(tripleExpr)
        case _ if (isDefined(ctx.multiElementOneOf())) => for {
          tripleExpr <- visitMultiElementOneOf(ctx.multiElementOneOf())
        } yield Some(tripleExpr)
        case _ => err(s"visitOneOfShape: unknown $ctx")
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

  override def
    visitUnaryShape(ctx: UnaryShapeContext): Builder[TripleExpr] =
    ctx match {
      case _ if (isDefined(ctx.include())) =>
        visitInclude(ctx.include())
      case _  => for {
        pl <- visitOpt(visitProductionLabel, ctx.productionLabel())
        te <- if (isDefined(ctx.encapsulatedShape()))
                visitEncapsulatedShape(ctx.encapsulatedShape())
        else if (isDefined(ctx.tripleConstraint()))
                 visitTripleConstraint(ctx.tripleConstraint())
             else err(s"visitUnaryShape: unknown $ctx")
    } yield te
  }

  // TODO: Where should I put ProductionLabels ?
  override def visitProductionLabel(ctx: ProductionLabelContext): Builder[Unit] =
    ok(())



  override def visitTripleConstraint(
                                      ctx: TripleConstraintContext): Builder[TripleExpr] =
    for {
      sense <- visitSenseFlags(ctx.senseFlags())
      predicate <- visitPredicate(ctx.predicate())
      shapeExpr <- visitInlineShapeExpression(ctx.inlineShapeExpression())
      cardinality <- getCardinality(ctx.cardinality())
      semActs <- visitSemanticActions(ctx.semanticActions())
      anns <- visitList(visitAnnotation,ctx.annotation())
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
          else Some(semActs)
      )
    }
  case class Sense(optInverse: Option[Boolean],
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
        case s: StarCardinalityContext => ok(star)
        case s: PlusCardinalityContext => ok(plus)
        case s: OptionalCardinalityContext => ok(optional)
        case s: RepeatCardinalityContext => visitRepeatCardinality(s)
        case _ => err(s"Not implemented cardinality ${ctx.getClass.getName}")
      } else ok((None, None))
  }

  override def
    visitRepeatCardinality(ctx: RepeatCardinalityContext
                          ): Builder[Cardinality] = {
    visitRepeatRange(ctx.repeatRange())
  }

  override def visitRepeatRange(
                                 ctx: RepeatRangeContext): Builder[Cardinality] = {
    for {
      min <- visitMin_range(ctx.min_range())
      max <- visitMax_range(ctx.max_range())
    } yield {
      val effectiveMax =
        if (min.isDefined && !max.isDefined) min.map(IntMax(_))
        else max
      (min, effectiveMax)
    }
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
                             ctx: IncludeContext): Builder[TripleExpr] =
    for {
    lbl <- visitShapeLabel(ctx.shapeLabel())
  } yield Inclusion(lbl)

  override def
     visitEncapsulatedShape(ctx: EncapsulatedShapeContext): Builder[TripleExpr] =
   for {
    tripleExpr <- visitInnerShape(ctx.innerShape())
    cardinality <- getCardinality(ctx.cardinality())
    annotations <- visitList(visitAnnotation, ctx.annotation())
    semActs <- visitSemanticActions(ctx.semanticActions())
  } yield extendTripleExpr(tripleExpr,cardinality,annotations,semActs)

  def extendTripleExpr(te: TripleExpr,
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
          else Some(sActs)
      )
      case eo: EachOf => eo.copy(
        optMin = cardinality._1,
        optMax = cardinality._2,
        annotations =
          if (anns.isEmpty) None
          else Some(anns),
        semActs =
          if (sActs.isEmpty) None
          else Some(sActs)
      )
      case so: OneOf => so.copy(
        optMin = cardinality._1,
        optMax = cardinality._2,
        annotations =
          if (anns.isEmpty) None
          else Some(anns),
        semActs =
          if (sActs.isEmpty) None
          else Some(sActs)
      )
      case i: Inclusion =>
        // TODO: Check how to extend include
        i
    }

  override def visitAnnotation(ctx: AnnotationContext):
     Builder[Annotation] = for {
    pred <- visitPredicate(ctx.predicate())
    obj <- if (isDefined(ctx.iri())) {
      visitIri(ctx.iri()).map{ iri => {
        val o : ObjectValue = IRIValue(iri)
        o
      } }
    } else {
       visitLiteral(ctx.literal()).map(v => v match {
         case o: ObjectValue => o
         case _ => throw new Exception(s"Unknown value in annotation $v")
       }
       )
      }
    } yield Annotation(pred,obj)


  override def visitInnerShape(ctx: InnerShapeContext): Builder[TripleExpr] =
    ctx match {
      case _ if isDefined(ctx.multiElementGroup()) => visitMultiElementGroup(ctx.multiElementGroup())
      case _ if isDefined(ctx.multiElementOneOf()) => visitMultiElementOneOf(ctx.multiElementOneOf())
      case _ => err("visitInnerShape. Unknown alternative")
    }

  override def visitSingleElementGroup(
                                        ctx: SingleElementGroupContext): Builder[TripleExpr] = {
    visitUnaryShape(ctx.unaryShape())
  }

  override def visitMultiElementGroup(
                                       ctx: MultiElementGroupContext): Builder[TripleExpr] =
   for {
    ses <- visitList(visitUnaryShape, ctx.unaryShape())
  } yield ses.length match {
    case 1 => ses.head
    case _ => EachOf(None, expressions = ses, None,None,None,None)
  }

  override def
     visitMultiElementOneOf(ctx: MultiElementOneOfContext):
        Builder[TripleExpr] = for {
    groups <- visitList(visitGroupShape, ctx.groupShape)
  } yield groups.length match {
    case 1 => groups.head
    case _ => OneOf(None,expressions = groups, None,None,None,None)
  }

  override def visitShapeLabel(ctx: ShapeLabelContext): Builder[ShapeLabel] = {
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
      case _ => throw new Exception("visitShapeLabel, no IRI and no BNode")
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

  // TODO: Resolve base taking into account previous base declarations ?
  override def visitBaseDecl(ctx: BaseDeclContext): Builder[IRI] = {
    val baseIri = extractIRIfromIRIREF(ctx.IRIREF().getText, None)
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

  def visitList[A, B](visitFn: A => Builder[B],
                      ls: java.util.List[A]): Builder[List[B]] =
    ls.asScala.toList.map(visitFn(_)).sequence

  def visitOpt[A,B](visitFn: A => Builder[B],
                    v: A): Builder[Option[B]] =
    if (isDefined(v)) visitFn(v).map(Some(_))
    else ok(None)

}
