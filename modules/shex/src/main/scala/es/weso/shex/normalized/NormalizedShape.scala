package es.weso.shex.normalized

import cats.implicits._
import es.weso.shex._

/**
 * A normalized shape consists of a list of slots where each slot is formed by a path and a list of constraints.
 * It can be represented as a map from a path to a list of constraints
 * @param slots a vector of pairs (Path, Vector[Constraint])
 */
case class NormalizedShape(slots: Map[Path, Vector[Constraint]],
                           closed: Boolean,
                          ) {
  lazy val paths: Set[Path] = slots.keySet
  lazy val hasRepeatedProperties: Boolean = slots.exists(_._2.size > 1)
}

object NormalizedShape {

  def fromShape(shape: Shape, schema: Schema): Either[String, NormalizedShape] = {
    val empty: Map[Path,Vector[Constraint]] = Map()
    for {
      cs <- shape.expression.fold(
        empty.asRight[String])(
        normalizeTripleExpr(_,empty, shape.extraPaths, schema)
      )
    } yield NormalizedShape(cs, shape.isClosed)
  }

  private def insert(m: Map[Path,Vector[Constraint]],
                     path: Path,
                     constraint: Constraint
                    ): Map[Path,Vector[Constraint]] =
    m.updated(path, m.get(path).fold(Vector(constraint))(_.appended(constraint)))

  private def normalizeTripleExpr(te: TripleExpr,
                                  cs: Map[Path,Vector[Constraint]],
                                  extraPaths: List[Path],
                                  schema: Schema
                                 ): Either[String,Map[Path,Vector[Constraint]]] = {
    te match {
      case _: Expr => Left(s"Contains an expr")
      case _: Inclusion => Left(s"Contains an inclusion")
      case eo: EachOf => {
        val zero = cs.asRight[String]
        def cmb(current: Either[String, Map[Path,Vector[Constraint]]],
                te: TripleExpr
               ): Either[String, Map[Path,Vector[Constraint]]] = for {
          cs <- current
          cs1 <- normalizeTripleExpr(te, cs, extraPaths, schema)
        } yield cs1
        eo.expressions.foldLeft(zero)(cmb)
      }
      case _: OneOf => Left(s"Contains a oneOf")
      case tc: TripleConstraint => tc.valueExpr match {
        case None => insert(cs, tc.path,
          Constraint(tc.valueExpr,
            extraPaths contains tc.path,
            Cardinality(tc.min,tc.max), tc.annotations, tc)
        ).asRight[String]
        case Some(se) =>
          if (se.hasNoReference(schema)) {
            insert(cs, tc.path, Constraint(tc.valueExpr,
              extraPaths contains tc.path,
              Cardinality(tc.min,tc.max),tc.annotations, tc)
            ).asRight[String]
          } else Left(s"$se contains a reference")
      }
    }
  }
}