package es.weso.shex.validator
import es.weso.shex._
import es.weso.rbe._

object table {

  type ConstraintsMap = Map[ConstraintRef,Option[ShapeExpr]]
  type PathsMap = Map[Path,Set[ConstraintRef]]
  case class ConstraintRef(n: Int) extends AnyVal {
    override def toString(): String = s"C$n"

  }

  // Constraints table
case class CTable(constraints: ConstraintsMap,
                    paths: PathsMap
                   ) {

 def addPath(p: Path, n: ConstraintRef): PathsMap =
   paths.updated(p, paths.get(p).getOrElse(Set()) + n)

 def mkTable(te: TripleExpr):(CTable,Rbe[ConstraintRef]) =
   mkTableAux(te, CTable.empty)

 def mkTableAux(te: TripleExpr, current: CTable): (CTable, Rbe[ConstraintRef]) = {
   te match {
     case e: EachOf => ???
     case s: SomeOf => ???
     case i: Inclusion => ???
     case tc: TripleConstraint => ???
   }
 }

}

object CTable {
  def empty: CTable = CTable(Map(),Map())


}
}