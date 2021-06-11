package es.weso.shex.converter

import cats.implicits._
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import cats._
import cats.implicits._
import es.weso.shex.Path

/**
  * A flat shape consists of a list of slots where each slot is formed by a path and a constraint.
  * It has no repeated properties
  * It can be represented as a map from a path to a constraint.
  * @param slots a vector of pairs (Path, Constraint)
  */
case class FlatShapeConversion(
    slots: Map[Path, Constraint], 
    closed: Boolean
    ) {
  lazy val paths: Set[Path]               = slots.keySet
  lazy val preds: Set[IRI]                = paths.collect { case Direct(p) => p }
  lazy val hasRepeatedProperties: Boolean = false
}

object FlatShapeConversion {

  def fromShape(
      shape: Shape, 
      schema: AbstractSchema
      ): Either[String, FlatShapeConversion] = {
    val empty: Map[Path, Constraint] = Map()
    for {
      cs <- shape.expression.fold(empty.asRight[String])(
        flattenTripleExpr(_, empty, shape.extraPaths, schema)
      )
    } yield FlatShapeConversion(cs, shape.isClosed)
  }

  private def flattenTripleExpr(
      te: TripleExpr,
      cs: Map[Path, Constraint],
      extraPaths: List[Path],
      schema: AbstractSchema
  ): Either[String, Map[Path, Constraint]] = {
  te match {
    case _: Expr      => Left(s"Contains an expr")
    case _: Inclusion => Left(s"Contains an inclusion")
    case eo: EachOf if !Cardinality.isDefault(eo.min,eo.max) => Left(s"Each of contains groupings")
    case _ if te.hasSemActs => Left(s"TripleExpr contains semantic actions")
    case eo: EachOf => {
      val zero = cs.asRight[String]
      def cmb(current: Either[String, Map[Path, Constraint]], te: TripleExpr): Either[String, Map[Path, Constraint]] =
        for {
          cs  <- current
          cs1 <- flattenTripleExpr(te, cs, extraPaths, schema)
        } yield cs1
      eo.expressions.foldLeft(zero)(cmb)
    }
    case _: OneOf => Left(s"Contains a oneOf")
    case tc: TripleConstraint =>
      if (cs.keySet contains tc.path) Left(s"Repeated properties: ${tc.path}")
      else
        tc.valueExpr match {
          case None =>
            cs.updated(
                tc.path,
                Constraint(tc.valueExpr, extraPaths contains tc.path, Cardinality(tc.min, tc.max), tc.annotations, tc)
              )
              .asRight[String]
          case Some(se) =>
            se match {
              case _ =>
                cs.updated(
                    tc.path,
                    Constraint(
                      tc.valueExpr,
                      extraPaths contains tc.path,
                      Cardinality(tc.min, tc.max),
                      tc.annotations,
                      tc
                    )
                  )
                  .asRight[String]
            }
        }
    }
  }

  implicit lazy val showFlatShape: Show[FlatShapeConversion] = new Show[FlatShapeConversion] {
    final def show(c: FlatShapeConversion): String = {
      s"FlatShape, closed: ${c.closed}\n${c.slots.map(showSlot).mkString("\n")}"
    }

    private def showSlot(pair: (Path, Constraint)): String = {
      val (path, c) = pair
      s"${path.show} -> ${c.show}"
    }
  }

}
