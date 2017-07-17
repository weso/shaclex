package es.weso.depgraphs

import scala.collection.SortedMap

trait DepGraph[Node] {

  def nodes: Set[Node]
  
  def addNode(node: Node): DepGraph[Node]

  def addPosEdge(node1: Node, node2: Node): DepGraph[Node]

  def addNegEdge(node1: Node, node2: Node): DepGraph[Node]

  def outEdges(node: Node): Either[String,Set[(PosNeg,Node)]]

  def empty: DepGraph[Node]
  
  def containsNegCycle: Boolean

}

object DepGraph {
  def empty[Node] = DepGraphJGraphT[Node]().empty
}

