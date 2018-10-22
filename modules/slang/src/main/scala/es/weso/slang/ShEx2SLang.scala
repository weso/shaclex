package es.weso.slang

import es.weso.rdf.nodes.IRI
import es.weso.shex.{
  BNodeKind => ShExBNodeKind,
  BNodeLabel => ShExBNodeLabel,
  IRIKind => ShExIRIKind,
  IRILabel => ShExIRILabel,
  IntMax => ShExIntMax,
  Max => ShExMax,
  Star => ShExStar, _}
import es.weso.slang
import es.weso.utils.EitherUtils._

trait ShEx2SLang {

 def shex2SLang(schema: Schema): Either[String,SchemaS] = for {
   keyValues <- sequence(schema.shapesMap.toList.map(cnvlabelShape(schema)))
 } yield SchemaS(keyValues.toMap)

  private def cnvlabelShape(schema: Schema)(pair: (ShapeLabel, ShapeExpr)): Either[String, (Label,SLang)] = {
    val (label,se) = pair
    for {
     lbl <- cnvLabel(label)
     s <- cnvShapeExpr(se,schema)
    } yield (lbl,s)
  }

  private def cnvLabel(lbl: ShapeLabel): Either[String,Label] = lbl match {
    case ShExBNodeLabel(bnode) => Right(slang.BNodeLabel(bnode))
    case ShExIRILabel(iri) => Right(slang.IRILabel(iri))
  }

  private def cnvShapeExpr(se: ShapeExpr, schema: Schema): Either[String,SLang] = se match {
    case ShapeAnd(_,ses, _, _) => for {
      ss <- sequence(ses.map(se => cnvShapeExpr(se,schema)))
    } yield ss.foldRight(SLang.strue)(And)
    case ShapeOr(_,ses,_,_) => for {
      ss <- sequence(ses.map(se => cnvShapeExpr(se,schema)))
    } yield ss.foldRight(SLang.sfalse)(SLang.or)
    case nk: NodeConstraint => for {
     s <- cnvNodeConstraint(nk)
    } yield s
    case ShapeNot(_,s,_,_) => for {
      sn <- cnvShapeExpr(s,schema)
    } yield Not(sn)
    case ShapeRef(ref,_,_) => for {
      lbl <- cnvLabel(ref)
    } yield Ref(lbl)
    case s: Shape => cnvShape(s,schema)
    case _ => Left(s"shex2slang: Unimplemented $se")
  }

  private def cnvNodeConstraint(nc: NodeConstraint): Either[String,SLang] = for {
    nks <- sequence(nc.nodeKind.map(cnvNodeKind(_)).toList)
    datatypes <- sequence(nc.datatype.map(cnvDatatype(_)).toList)
    // TODO convert the rest: xsfacets, values...
    r <- {
      val maybeS : Option[SLang] = (nks ++ datatypes).reduceOption(SLang.and)
      maybeS match {
        case None => Left(s"cnvNodeConstraint($nc): No values in constraint")
        case Some(s) => Right(s)
      }
    }
   } yield r

  private def cnvNodeKind(nk: NodeKind): Either[String,SLang] = nk match {
    case ShExIRIKind => Right(slang.IRIKind)
    case ShExBNodeKind => Right(slang.BNodeKind)
    case _ => Left(s"shex2slang (cnvNodeKind): Unimplemented $nk")
  }

  private def cnvDatatype(dt: IRI): Either[String,SLang] =
    Right(Datatype(dt))

  // TODO: Handle Closed, Extras, etc....
  private def cnvShape(s: Shape, schema: Schema): Either[String,SLang] = s.expression match {
    case None => Right(SLang.strue)
    case Some(expr) => cnvTripleExpr(expr,schema)
  }

  private def cnvTripleExpr(te: TripleExpr, schema: Schema): Either[String,SLang] = te match {
    case eo: EachOf => for {
      es <- sequence(eo.expressions.map(cnvTripleExpr(_,schema)))
      preds = eo.predicates(schema)
    } yield And(
      es.foldRight(SLang.strue)(And),
      Not(QualifiedArc(NoPreds(preds.toSet), SLang.strue, Card.oneStar))
    )

    case tc: TripleConstraint => for {
      s <- tc.valueExpr.map(cnvShapeExpr(_,schema)).getOrElse(Right(SLang.strue))
      card = cnvCard(tc.min,tc.max)
    } yield
      And(
        QualifiedArc(Pred(tc.predicate),s,card),
        Not(QualifiedArc(Pred(tc.predicate), Not(s), Card.oneStar))
      )
    case _ => Left(s"shex2slang (cnvTripleExpr): Unimplemented $te")
  }

  private def cnvCard(min: Int, max: ShExMax): Card =
    Card(min,max match {
      case ShExIntMax(n) => IntMax(n)
      case ShExStar => Star
    })

}