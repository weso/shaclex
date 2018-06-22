package es.weso.shex

import cats._
import cats.implicits._
import es.weso.rdf.nodes.RDFNode

case class VarTable(table: Map[VarName,List[RDFNode]]) {
  def set(name: VarName, nodes: List[RDFNode]): VarTable =
    VarTable(table.updated(name,nodes))

  def get(name: VarName): List[RDFNode] = table.get(name).getOrElse(List())

}

object VarTable {
  implicit val ctxMonoid: Monoid[VarTable] = new Monoid[VarTable] {
    def combine(e1: VarTable, e2: VarTable): VarTable = VarTable(e1.table |+| e2.table)
    def empty: VarTable = VarTable(Monoid[Map[VarName,List[RDFNode]]].empty)
  }

  implicit val ctxShow: Show[VarTable] = new Show[VarTable] {
    def show(e: VarTable): String = "{" +
      e.table.map{ case (v,ls) => s"${v.show}->[${ls.map(_.show).mkString(",")}]}" } +
      "}"
  }

}