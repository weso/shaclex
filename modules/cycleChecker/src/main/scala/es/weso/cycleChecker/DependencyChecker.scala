package es.weso.cycleChecker
import cats._, data._
import cats.implicits._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge     // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts

abstract class DependencyKind[X] extends LDiEdge[X]
case object Positive extends DependencyKind[Nothing]
case object Negative extends DependencyKind[Nothing]

abstract class CycleChecker[Node] {
  val graph: Graph[Node, DependencyKind]
  def addNode(n:Node): CycleChecker[Node]
  def addDependency(n1:Node, n2:Node, kind: DependencyKind[Nothing]): CycleChecker[Node]
}