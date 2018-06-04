package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.collection.Bag
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.ShapeExpr
import es.weso.shex.validator.table.CTable
import es.weso.utils.SeqUtils.filterOptions

case class CandidateLine(values: List[(Arc,ConstraintRef)]) {
  def mkBag: Bag[ConstraintRef] = Bag.toBag(values.map(_._2))

  def nodeShapes(table: CTable): List[(RDFNode, ShapeExpr)] =
    filterOptions(values.map {
      case (arc, cref) => (arc.node, table.getShapeExpr(cref))
    })
}

object CandidateLine {

  implicit lazy val showCandidateLine = new Show[CandidateLine] {
    def show(cl: CandidateLine): String = {
      s"[${cl.values.map{ case (arc,cref) => (arc.show, cref.show)}.mkString(",")}]"
    }
  }

}
