package es.weso.rbe

/**
 * 
 */
case class Table[Edge,Node,Label,Err,Evidence](
    constraints: Map[ConstraintRef,NodeShape[Node,Label,Err,Evidence]],
    edges: Map[DirectedEdge[Edge],Set[ConstraintRef]],
    elems: Int) {
  
  def addEdge(
      e: DirectedEdge[Edge], 
      n: ConstraintRef): Map[DirectedEdge[Edge],Set[ConstraintRef]] = {
    edges.updated(e, 
        edges.get(e).getOrElse(Set()) + n)
  }
  
  
}

object Table {
  def empty[Edge,Node,Label,Err,Evidence]: Table[Edge,Node,Label,Err,Evidence] =
    Table(Map(),Map(),0)
    
def mkTable[Edge,Node,Label,Err,Evidence](
    shape: SingleShape[DirectedEdge[Edge], Node, Label, Err, Evidence]): 
     (Table[Edge,Node,Label,Err,Evidence], Rbe[ConstraintRef]) = mkTableAux(shape.rbe, Table.empty)

// Todo: Rewrite this code to make it tailrec     
private def mkTableAux[Edge,Node,Label,Err,Evidence](
    rbe: Rbe[(DirectedEdge[Edge], NodeShape[Node, Label, Err, Evidence])],
    current: Table[Edge,Node,Label,Err,Evidence]): (Table[Edge,Node,Label,Err,Evidence], Rbe[ConstraintRef]) = {
    rbe match {
      case Empty => (current, Empty)
      case Symbol((p, c), m, n) => {
        val newElem = current.elems + 1
        val cref = ConstraintRef(newElem)
        val newTable = current.copy(
          elems = newElem,
          constraints = current.constraints + (cref -> c),
          edges = current.addEdge(p, cref))
        (newTable, Symbol(cref, m, n))
      }
      case And(s1, s2) => {
        val (t1, r1) = mkTableAux(s1, current)
        val (t2, r2) = mkTableAux(s2, t1)
        (t2, And(r1, r2))
      }
      case Or(s1, s2) => {
        val (t1, r1) = mkTableAux(s1, current)
        val (t2, r2) = mkTableAux(s2, t1)
        (t2, Or(r1, r2))
      }
      case Plus(s) => {
        val (t, r) = mkTableAux(s, current)
        (t, Plus(r))
      }
      case Star(s) => {
        val (t, r) = mkTableAux(s, current)
        (t, Star(r))
      }
      case Repeat(s, n, m) => {
        val (t, r) = mkTableAux(s, current)
        (t, Repeat(r, n, m))
      }
      case _ => throw SESchemaException(s"mkTableAux: Unsupported rbe: $rbe")
    }
  }

}    
