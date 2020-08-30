package es.weso.shex.linter

import cats._
import cats.data._
import cats.implicits._
import es.weso.shex._

object CountUsages {

  def countUsages(schema: Schema): Either[String, Map[ShapeLabel, Int]] = {
    val (visited, eitherUsages) = countUsagesSchema.value.run(initialVisited).run(schema)

    eitherUsages
  }

  private lazy val initialVisited: Visited = Set()
  type Visited         = Set[ShapeLabel]
  type SchemaReader[A] = ReaderT[Id, Schema, A]
  type S[A]            = StateT[SchemaReader, Visited, A]
  type Err             = String
  type Result[A]       = EitherT[S, Err, A]

  private def ok[A](x: A): Result[A]         = EitherT.pure[S, Err](x) // StateT[SchemaReader,Visited,A].pure(x)

  private def err[A](str: String): Result[A] = EitherT.leftT[S, A](str)

  private def getVisited: Result[Visited]    = EitherT.liftF[S, Err, Visited](StateT.get[SchemaReader, Visited])

  private def modifyVisited(fn: Visited => Visited): Result[Unit] =
    EitherT.liftF[S, Err, Unit](StateT.modify[SchemaReader, Visited](fn))

  private def addVisited(lbl: ShapeLabel): Result[Unit] =
    modifyVisited(visited => visited + lbl)

  private def getSchema: Result[Schema] =
    EitherT.liftF[S, Err, Schema](StateT.liftF[SchemaReader, Visited, Schema](Kleisli.ask))

  private def sequence[A](ls: List[Result[A]]): Result[List[A]] = ls.sequence[Result, A]

  private def getShape(lbl: ShapeLabel): Result[ShapeExpr] =
    for {
      schema <- getSchema
      se     <- schema.getShape(lbl).fold(e => err[ShapeExpr](e), ok(_))
    } yield se

   /* This method may be useful to count usages of tripleExpr labels
   private def tripleExprBNodeLabels(schema: Schema): List[ShapeLabel] =
    schema.optTripleExprMap.getOrElse(Map()).keys.toList // .filter(isBNode(_))
    */

  private def countUsagesSchema: Result[Map[ShapeLabel, Int]] =
    for {
      schema <- getSchema
//      usages <- sequence(tripleExprBNodeLabels(schema).map(countUsagesLabel(_)))
      usages <- sequence(schema.labels.map(countUsagesLabel))
    } yield {
      usages.toMap
    }

  private def countUsagesLabel(lbl: ShapeLabel): Result[(ShapeLabel, Int)] = {
    def zero = ok(0)
    def comb(r: Result[Int], se: ShapeExpr): Result[Int] =
      for {
        c <- countUsagesLabelShapeExpr(lbl, se)
        n <- r
      } yield n + c
    for {
      schema <- getSchema
      count  <- schema.shapeList.foldLeft(zero)(comb)
    } yield {
      (lbl, count)
    }
  }

  private def countUsagesLabelShapeExpr(lbl: ShapeLabel, se: ShapeExpr): Result[Int] =
    se match {
      case sa: ShapeAnd       => sequence(sa.shapeExprs.map(countUsagesLabelShapeExpr(lbl, _))).map(_.sum)
      case so: ShapeOr        => sequence(so.shapeExprs.map(countUsagesLabelShapeExpr(lbl, _))).map(_.sum)
      case sn: ShapeNot       => countUsagesLabelShapeExpr(lbl, sn.shapeExpr)
      case nk: NodeConstraint => ok(0)
      case s: Shape           => s.expression.fold(ok(0))(countUsagesLabelTripleExpr(lbl, _))
      case s: ShapeExternal   => ok(0)
      case sr: ShapeRef => {
        val lbl = sr.reference
        for {
          visited <- getVisited
          n <- if (visited contains lbl) ok(0)
          else
            for {
              _     <- addVisited(lbl)
              shape <- getShape(lbl)
              n     <- countUsagesLabelShapeExpr(lbl, shape)
            } yield n
        } yield n
      }
    }

  private def countUsagesLabelTripleExpr(lbl: ShapeLabel, te: TripleExpr): Result[Int] =
    te match {
      case e: EachOf => sequence(e.expressions.map(countUsagesLabelTripleExpr(lbl, _))).map(_.sum)
      case e: OneOf  => sequence(e.expressions.map(countUsagesLabelTripleExpr(lbl, _))).map(_.sum)
      case Inclusion(includeLbl) =>
        if (lbl == includeLbl) ok(1)
        else ok(0)
      case e: Expr              => ok(0)
      case tc: TripleConstraint => tc.valueExpr.fold(ok(0))(countUsagesLabelShapeExpr(lbl, _))
    }


}
