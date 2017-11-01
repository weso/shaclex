package es.weso.shex.validator

import cats._
import es.weso.rbe.interval.{IntLimit, IntOrUnbounded, Unbounded}
import es.weso.rbe.{Schema => _, Star => _, Direct => _, _}
import es.weso.rdf.nodes.IRI
import es.weso.shex._

/* Candidates table */
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

  object ConstraintRef {
    implicit lazy val orderingConstraintRef = new Ordering[ConstraintRef] {
      def compare(c1: ConstraintRef, c2: ConstraintRef): Int = {
        Ordering[Int].compare(c1.n, c2.n)
      }
    }

    implicit lazy val showConstraintRef =
      Show.fromToString[ConstraintRef]

  }

  // Constraints table
  case class CTable(
    constraints: ConstraintsMap,
    paths: PathsMap,
    elems: Int,
    schema: Schema) {

    private[validator] def addPath(p: Path, n: ConstraintRef): PathsMap =
      paths.updated(p, paths.get(p).getOrElse(Set()) + n)

    private[validator] def getShapeExpr(cref: ConstraintRef): Option[ShapeExpr] = {
      constraints.get(cref).map(ce => ce match {
        case Pos(se) => se
        case Neg(se) => ShapeNot(None, se)
      })
    }

    private[validator] lazy val isAmbiguous: Boolean = {
      paths.values.map(_.size).exists(_ > 1)
    }

    private[validator] def addConstraint(path: Path, expr: CheckExpr): (CTable, ConstraintRef) = {
      val cref = ConstraintRef(this.elems)
      val newTable = this.copy(
        elems = this.elems + 1,
        constraints = this.constraints + (cref -> expr),
        paths = addPath(path,cref)
      )
      (newTable,cref)
    }
  }

  object CTable {
    def empty: CTable = CTable(Map(), Map(), 0, Schema.empty)

    def simplify(rbe: Rbe_): Rbe_ = {
      rbe match {
        case And(Empty, e1) => e1
        case Or(Empty, e1) => e1
        case And(e1, e2) => And(simplify(e1), e2)
        case Or(e1, e2) => Or(simplify(e1), e2)
        case e => e
      }
    }

    private[validator] def extendWithExtras(pair: ResultPair, te: TripleExpr, extras: List[IRI]): ResultPair = {
      val zero: ResultPair = pair
      def combine(current: ResultPair, extra: IRI): ResultPair = {
        def appearances(iri: IRI): List[ShapeExpr] = te match {
          case tc: TripleConstraint => if (tc.predicate == iri) tc.valueExpr.toList
          else List()
          case eachOf: EachOf => List() // TODO
          case oneOf: OneOf => List() // TODO
          case i: Inclusion => List() // TODO
        }
        val s: ShapeExpr = ShapeNot(None,ShapeOr(None,appearances(extra)))
        val (table,rbe) = current
        val (newTable,cref) = table.addConstraint(Direct(extra),Pos(s))
        val newRbe = And(rbe,Symbol(cref,0,Unbounded))
        (newTable,newRbe)
      }
      extras.foldLeft(zero)(combine)
    }

    private[validator] def mkTable(te: TripleExpr, extras: List[IRI]): ResultPair = {
      val pair = mkTableAux(te, CTable.empty)
      extendWithExtras(pair, te, extras)
    }

    private def mkTableAux(te: TripleExpr, current: CTable): ResultPair = {
      te match {
        case e: EachOf => {
          val zero: ResultPair = (current, Empty)
          def comb(pair: ResultPair, currentTe: TripleExpr): ResultPair = {
            val (currentTable, currentRbe) = pair
            val (newTable, newRbe) = mkTableAux(currentTe, currentTable)
            (newTable, And(currentRbe, newRbe))
          }
          val (newTable, rbe) = e.expressions.foldLeft(zero)(comb)
          val simplifiedRbe: Rbe_ = simplify(rbe) // e.expressions.map(_ ).reduce(And)
          val groupRbe =
            if (Cardinality.isDefault(e.min, e.max)) simplifiedRbe
            else Repeat(simplifiedRbe, e.min, max2IntOrUnbounded(e.max))
          (newTable, groupRbe)
        }
        case e: OneOf => {
          val zero: ResultPair = (current, Empty)
          def comb(pair: ResultPair, currentTe: TripleExpr): ResultPair = {
            val (currentTable, currentRbe) = pair
            val (newTable, newRbe) = mkTableAux(currentTe, currentTable)
            (newTable, Or(currentRbe, newRbe))
          }
          val (newTable, rbe) = e.expressions.foldLeft(zero)(comb)
          val simplifiedRbe: Rbe_ = simplify(rbe)
          val groupRbe =
            if (Cardinality.isDefault(e.min, e.max))
              simplifiedRbe
            else Repeat(simplifiedRbe, e.min, max2IntOrUnbounded(e.max))
          (newTable, groupRbe)
        }
        case i: Inclusion =>
          throw new Exception("CTable: Not implemented table generation for inclusion")

        case tc: TripleConstraint => {
          val valueExpr: CheckExpr = if (tc.negated) {
            tc.valueExpr match {
              case Some(se) => Neg(se)
              case None => Neg(ShapeExpr.any)
            }
          } else tc.valueExpr match {
            case Some(se) => Pos(se)
            case None => Pos(ShapeExpr.any)
          }
          val (newTable, cref) = current.addConstraint(tc.path, valueExpr)
          val posSymbol = Symbol(cref, tc.min, max2IntOrUnbounded(tc.max))
          val symbol = if (tc.negated) {
            Repeat(posSymbol, 0, IntLimit(1))
          } else posSymbol
          (newTable, symbol)
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
      def showConstraints(cs: ConstraintsMap): String = {
        def combine(s: List[String], current: (ConstraintRef, CheckExpr)): List[String] = {
          val (cref,expr) = current
          s"${cref.toString}->${expr.toString}" :: s
        }
        cs.foldLeft(List[String]())(combine).mkString(",")
      }
      def showPaths(pm: PathsMap): String = {
        def combine(s: List[String], current: (Path,Set[ConstraintRef])): List[String] = {
          val (path,cs) = current
          s"${path.toString}->[${cs.map(_.toString).mkString(",")}]" :: s
        }
        pm.foldLeft(List[String]())(combine).mkString(",")
      }
      s"""Constraints: ${showConstraints(table.constraints)}\nPaths: ${table.paths.toString}""".stripMargin
    }
  }
}