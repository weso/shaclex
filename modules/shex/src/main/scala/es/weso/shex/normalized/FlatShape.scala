package es.weso.shex.normalized

import cats.implicits._
import es.weso.shex._

/**
 * A flat shape consists of a list of slots where each slot is formed by a path and a constraint.
 * It has no repeated properties
 * It can be represented as a map from a path to a constraint.
 * @param slots a vector of pairs (Path, Constraint)
 */
case class FlatShape(slots: Map[Path, Constraint],
                           closed: Boolean,
                          ) {
  lazy val paths: Set[Path] = slots.keySet
  lazy val hasRepeatedProperties: Boolean = false
}

object FlatShape {

  def fromShape(shape: Shape, schema: Schema): Either[String, FlatShape] = {
      val empty: Map[Path,Constraint] = Map()
      for {
        cs <- shape.expression.fold(
          empty.asRight[String])(
          flattenTripleExpr(_,empty, shape.extraPaths, schema)
        )
      } yield FlatShape(cs, shape.isClosed)
    }

    private def flattenTripleExpr(te: TripleExpr,
                                  cs: Map[Path,Constraint],
                                  extraPaths: List[Path],
                                  schema: Schema
                                 ): Either[String,Map[Path,Constraint]] = te match {
      case _: Expr => Left(s"Contains an expr")
      case _: Inclusion => Left(s"Contains an inclusion")
      case eo: EachOf => {
        val zero = cs.asRight[String]
        def cmb(current: Either[String, Map[Path,Constraint]],
                te: TripleExpr
               ): Either[String, Map[Path,Constraint]] = for {
          cs <- current
          cs1 <- flattenTripleExpr(te, cs, extraPaths, schema)
        } yield cs1
        eo.expressions.foldLeft(zero)(cmb)
      }
      case _: OneOf => Left(s"Contains a oneOf")
      case tc: TripleConstraint => if (cs.keySet contains tc.path) Left(s"Repeated properties: ${tc.path}")
        else tc.valueExpr match {
        case None => cs.updated(tc.path,
          Constraint(tc.valueExpr,
            extraPaths contains tc.path,
            Cardinality(tc.min,tc.max), tc.annotations, tc)
        ).asRight[String]
        case Some(se) =>
          if (se.hasNoReference(schema) && se.isSimple(schema)) {
            cs.updated(tc.path, Constraint(tc.valueExpr,
              extraPaths contains tc.path,
              Cardinality(tc.min,tc.max),tc.annotations, tc)
            ).asRight[String]
          } else Left(s"$se is not simple or contains a reference")
      }
    }
}

