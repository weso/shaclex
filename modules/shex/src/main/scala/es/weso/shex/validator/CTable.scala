package es.weso.shex.validator

import cats._
import es.weso.rbe.interval.{IntLimit, IntOrUnbounded, Unbounded}
import es.weso.rbe.{Schema => _, Star => _, _}
import es.weso.shex._


object table {

  type Rbe_ = Rbe[ConstraintRef]
  type ConstraintsMap = Map[ConstraintRef, CheckExpr]
  type PathsMap = Map[Path, Set[ConstraintRef]]
  type ResultPair = (CTable, Rbe_)

  trait CheckExpr
  case class Pos(se: ShapeExpr) extends CheckExpr
  case class Neg(se: ShapeExpr) extends CheckExpr

  case class ConstraintRef(n: Int) extends AnyVal {
    override def toString(): String = s"C$n"
  }

  implicit lazy val orderingConstraintRef = new Ordering[ConstraintRef] {
    def compare(c1: ConstraintRef, c2: ConstraintRef): Int = {
      Ordering[Int].compare(c1.n, c2.n)
    }
  }

  // Constraints table
  case class CTable(constraints: ConstraintsMap,
                    paths: PathsMap,
                    elems: Int,
                    schema: Schema
                   ) {

    def addPath(p: Path, n: ConstraintRef): PathsMap =
      paths.updated(p, paths.get(p).getOrElse(Set()) + n)

    def getShapeExpr(cref: ConstraintRef): Option[ShapeExpr] = {
      constraints.get(cref).map(ce => ce match {
        case Pos(se) => se
        case Neg(se) => ShapeNot(se)
      })
    }

    lazy val isAmbiguous: Boolean = {
      paths.values.map(_.size).exists(_ > 1)
    }

  }

  object CTable {
    def empty: CTable = CTable(Map(), Map(), 0, Schema.empty)

    def simplify(rbe: Rbe_): Rbe_ = {
      rbe match {
        case And(Empty,e1) => e1
        case Or(Empty,e1) => e1
        case And(e1,e2) => And(simplify(e1),e2)
        case Or(e1,e2) => Or(simplify(e1),e2)
        case e => e
      }
    }

    def mkTable(te: TripleExpr): ResultPair =
      mkTableAux(te, CTable.empty)

    def mkTableAux(te: TripleExpr, current: CTable): ResultPair = {
      te match {
        case e: EachOf => {
          val zero: ResultPair = (current, Empty)
          def comb(pair: ResultPair, currentTe: TripleExpr): ResultPair = {
            val (currentTable, currentRbe) = pair
            val (newTable, newRbe) = mkTableAux(currentTe, currentTable)
            (newTable, And(currentRbe,newRbe))
          }
          val (newTable,rbe) = e.expressions.foldLeft(zero)(comb)
          val simplifiedRbe: Rbe_ = simplify(rbe) // e.expressions.map(_ ).reduce(And)
          val groupRbe =
            if (Cardinality.isDefault(e.min, e.max)) simplifiedRbe
            else Repeat(simplifiedRbe, e.min, max2IntOrUnbounded(e.max))
          (newTable,groupRbe)
        }
        case e: SomeOf => {
          val zero: ResultPair = (current, Empty)
          def comb(pair: ResultPair, currentTe: TripleExpr): ResultPair = {
            val (currentTable, currentRbe) = pair
            val (newTable, newRbe) = mkTableAux(currentTe, currentTable)
            (newTable, Or(currentRbe,newRbe))
          }
          val (newTable,rbe) = e.expressions.foldLeft(zero)(comb)
          val simplifiedRbe: Rbe_ = simplify(rbe)
          val groupRbe =
            if (Cardinality.isDefault(e.min, e.max))
              simplifiedRbe
            else Repeat(simplifiedRbe, e.min, max2IntOrUnbounded(e.max))
          (newTable,groupRbe)
        }
        case i: Inclusion =>
          throw new Exception("CTable: Not implemented table generation for inclusion")

        case tc: TripleConstraint => {
          val newElems = current.elems + 1
          val cref = ConstraintRef(newElems)
          val valueExpr: CheckExpr = if (tc.negated) {
            tc.valueExpr match {
              case Some(se) => Neg(se)
              case None => Neg(ShapeExpr.any)
            }
          } else tc.valueExpr match {
            case Some(se) => Pos(se)
            case None => Pos(ShapeExpr.any)
          }
          val newTable = current.copy(
            elems = newElems,
            constraints =
              current.constraints +
                (cref -> valueExpr ),
            paths = current.addPath(tc.path, cref)
          )
          val symbol = /*if (tc.negated) {
            // Adjust cardinality to be 0,0
            Symbol(cref, 0, IntLimit(0))
          } else */
            Symbol(cref, tc.min, max2IntOrUnbounded(tc.max))
          println(s"Making table for tc $tc. Negated: ${tc.negated}. Symbol: $symbol")
          (newTable,symbol)
        }
      }
    }

    def max2IntOrUnbounded(m: Max): IntOrUnbounded = {
      m match {
        case IntMax(v) => IntLimit(v)
        case Star => Unbounded
      }
    }
  }

  implicit lazy val showCTable = new Show[CTable] {
    override def show(table: CTable) = {
      s"""CTable { constraints: ${table.constraints.toString}\n
         | paths: ${table.paths.toString}""".stripMargin
    }
  }
}