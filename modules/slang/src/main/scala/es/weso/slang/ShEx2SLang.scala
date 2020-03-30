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
// import es.weso.utils.EitherUtils._
import cats.effect.IO
import cats.data._
import cats.implicits._

trait ShEx2SLang {

 def shex2SLang(schema: Schema): EitherT[IO,String, SchemaS] = for {
   keyValues <- schema.shapesMap.toList.map(cnvlabelShape(schema)).sequence
 } yield SchemaS(keyValues.toMap)

  private def cnvlabelShape(schema: Schema)(pair: (ShapeLabel, ShapeExpr)): EitherT[IO, String, (Label,SLang)] = {
    val (label,se) = pair
    for {
     lbl <- cnvLabel(label)
     s <- cnvShapeExpr(se,schema)
    } yield (lbl,s)
  }

  private def cnvLabel(lbl: ShapeLabel): EitherT[IO,String, Label] = lbl match {
    case ShExBNodeLabel(bnode) => ok(slang.BNodeLabel(bnode))
    case ShExIRILabel(iri) => ok(slang.IRILabel(iri))
    case Start => err(s"Unimplemented conversion of Start to SLang")
  }

  private def cnvShapeExpr(se: ShapeExpr, schema: Schema): EitherT[IO,String, SLang] = se match {
    case ShapeAnd(_,ses, _, _) => for {
      ss <- ses.map(se => cnvShapeExpr(se,schema)).sequence
    } yield ss.foldRight(SLang.strue)(And)
    case ShapeOr(_,ses,_,_) => for {
      ss <- ses.map(se => cnvShapeExpr(se,schema)).sequence
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
    case _ => err(s"shex2slang: Unimplemented $se")
  }

  private def cnvNodeConstraint(nc: NodeConstraint): EitherT[IO,String,SLang] = for {
    nks <- nc.nodeKind.map(cnvNodeKind(_)).sequence
    datatypes <- nc.datatype.map(cnvDatatype(_)).sequence
    // TODO convert the rest: xsfacets, values...
    r <- {
      val maybeS : Option[SLang] = (nks ++ datatypes).reduceOption(SLang.and)
      maybeS match {
        case None => err(s"cnvNodeConstraint($nc): No values in constraint")
        case Some(s) => ok(s)
      }
    }
   } yield r

  private def cnvNodeKind(nk: NodeKind): EitherT[IO,String,SLang] = nk match {
    case ShExIRIKind => ok(slang.IRIKind)
    case ShExBNodeKind => ok(slang.BNodeKind)
    case _ => err(s"shex2slang (cnvNodeKind): Unimplemented $nk")
  }

  private def ok[A](x:A): EitherT[IO,String,A] = EitherT.fromEither(x.asRight)
  private def err[A](msg: String): EitherT[IO,String,A] = EitherT.fromEither[IO](msg.asLeft[A])

  private def cnvDatatype(dt: IRI): EitherT[IO,String,SLang] =
    ok(Datatype(dt))

  // TODO: Handle Closed, Extras, etc....
  private def cnvShape(s: Shape, schema: Schema): EitherT[IO,String,SLang] = s.expression match {
    case None => EitherT.fromEither(SLang.strue.asRight)
    case Some(expr) => cnvTripleExpr(expr,schema)
  }

  private def cnvTripleExpr(te: TripleExpr, schema: Schema): EitherT[IO,String,SLang] = te match {
    case eo: EachOf => for {
      es <- eo.expressions.map(cnvTripleExpr(_,schema)).sequence
      preds = eo.predicates(schema)
    } yield And(
      es.foldRight(SLang.strue)(And),
      Not(QualifiedArc(NoPreds(preds.toSet), SLang.strue, Card.oneStar))
    )

    case tc: TripleConstraint => for {
      s <- tc.valueExpr.map(cnvShapeExpr(_,schema)) match {
        case None => EitherT.fromEither[IO](SLang.strue.asRight[String])
        case Some(r) => r
      }
      card = cnvCard(tc.min,tc.max)
    } yield
      And(
        QualifiedArc(Pred(tc.predicate),s,card),
        Not(QualifiedArc(Pred(tc.predicate), Not(s), Card.oneStar))
      )
    case _ => EitherT.fromEither(s"shex2slang (cnvTripleExpr): Unimplemented $te".asLeft)
  }

  private def cnvCard(min: Int, max: ShExMax): Card =
    Card(min,max match {
      case ShExIntMax(n) => IntMax(n)
      case ShExStar => Star
    })

}