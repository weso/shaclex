package es.weso.shex
import cats.implicits._

case class Constraint(shape: Option[ShapeExpr], hasExtra: Boolean, min: Int, max: Max)

/**
* A normalized shape has no repeated paths and no semantic actions.
 * It can be represented as a map from a path to a constraint.
 * @param constraints a map from a Path to a constraint
 */
case class NormalizedShape(constraints: Map[Path, Constraint], closed: Boolean) {
  lazy val paths: Set[Path] = constraints.keySet
}

object NormalizedShape {
  def fromShape(shape: Shape): Either[String, NormalizedShape] = {
    val empty: Map[Path,Constraint] = Map()
    for {
      cs <- shape.expression.fold(
        empty.asRight[String])(
        normalizeTripleExpr(_,empty, shape.extraPaths)
      )
    } yield NormalizedShape(cs, shape.isClosed)
  }

  private def normalizeTripleExpr(te: TripleExpr,
                                  cs: Map[Path, Constraint],
                                  extraPaths: List[Path]
                                 ): Either[String,Map[Path,Constraint]] = te match {
    case Expr(id,e) => Left(s"Contains an expr")
    case i: Inclusion => Left(s"Constains an inclusion")
    case eo: EachOf => {
      val zero = cs.asRight[String]
      def cmb(current: Either[String,Map[Path,Constraint]],
              te: TripleExpr
             ): Either[String, Map[Path,Constraint]] = for {
        cs <- current
        cs1 <- normalizeTripleExpr(te,cs,extraPaths)
      } yield cs1
      eo.expressions.foldLeft(zero)(cmb)
    }
    case oo: OneOf => Left(s"Constains a oneOf")
    case tc: TripleConstraint =>
      if (cs.keySet contains tc.path) Left(s"Repeated path: ${tc.path}")
      else cs.updated(tc.path,
        Constraint(tc.valueExpr,
        extraPaths contains tc.path,
        tc.min,
        tc.max
        )
      ).asRight[String]
  }
}