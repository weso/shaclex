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

  def showEdges(showNode: Node => String = (x => x.toString)): String

  /**
  * Checks if this graph is isomorphic with another one
  * @param other the other dependency graph
  * @return Left(msg) if it is not isomorphic or Right(()) if it is isomorphic
  */
  def isomorphicWith(other: DepGraph[Node]): Either[String,Unit]
}

object DepGraph {
  def empty[Node] = DepGraphJGraphT[Node]().empty

  def makeGraph[Node](deps: Set[(Node,Set[(PosNeg,Node)])]): DepGraph[Node] = {
    def combine(pair: (Node, Set[(PosNeg,Node)]), g: DepGraph[Node]): DepGraph[Node] = {
      val (source,edges) = pair
      edges.foldRight(g)((pair,current) => {
        val (posNeg,target) = pair
        current.addEdge(source,posNeg,target)
      })
    }
    deps.foldRight(empty[Node])(combine)
  }
}

