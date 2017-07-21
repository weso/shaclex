package es.weso.depgraphs

import scala.collection.SortedMap

trait DepGraph[Node] {

  def nodes: Set[Node]
  
  def addNode(node: Node): DepGraph[Node]

  def addEdge(node1: Node, posNeg: PosNeg, node2: Node): DepGraph[Node]

  def addPosEdge(node1: Node, node2: Node): DepGraph[Node] =
    addEdge(node1,Pos,node2)

  def addNegEdge(node1: Node, node2: Node): DepGraph[Node] =
    addEdge(node1,Neg,node2)

  def outEdges(node: Node): Either[String,Set[(PosNeg,Node)]]

  def empty: DepGraph[Node]
  
  def containsNegCycle: Boolean = {
    !negCycles.isEmpty
  }

  def negCycles: Set[Set[Node]]

}

object DepGraph {
  def empty[Node] = DepGraphJGraphT[Node]().empty
}

