package es.weso.collection

import org.jgrapht.DirectedGraph
import org.jgrapht.graph.DefaultDirectedGraph
import collection.JavaConverters._

abstract class PosNeg
case object Pos extends PosNeg
case object Neg extends PosNeg


class GraphJGraphT[Node]() extends Graph[Node,PosNeg] {
  val graph : DirectedGraph[Node,PosNeg] =
    new DefaultDirectedGraph[Node,PosNeg](classOf[PosNeg])

  def removeAllEdges() {
   val edges: java.util.Set[PosNeg] = graph.edgeSet
   graph.removeAllEdges(edges)
  }
  
  def empty : Graph[Node,PosNeg] = {
   removeAllEdges()
   this
  }
  
  def addNode(n: Node): Graph[Node,PosNeg] = {
    graph.addVertex(n)
    this
  }

  def nodes: Set[Node] = {
    // graph.vertexSet()
    Set()
  }
}

object GraphMap {
}