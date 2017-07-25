package es.weso.depgraphs

import org.jgrapht.DirectedGraph
import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.graph.DirectedSubgraph

import org.jgrapht.alg.KosarajuStrongConnectivityInspector
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm
import org.jgrapht.alg.ConnectivityInspector 
import collection.JavaConverters._

case class DepGraphJGraphT[Node]() extends DepGraph[Node] {

  case class Edge(source: Node, posNeg: PosNeg, target: Node)

  val graph : DirectedGraph[Node,Edge] =
    new DefaultDirectedGraph[Node,Edge](classOf[Edge])

  def removeAllEdges() {
   val edges: java.util.Set[Edge] = graph.edgeSet
   graph.removeAllEdges(edges)
  }
  
  override def empty : DepGraph[Node] = {
   removeAllEdges()
   this
  }
  
  override def addNode(n: Node): DepGraph[Node] = {
    graph.addVertex(n)
    this
  }

  override def nodes: Set[Node] = {
    graph.vertexSet.asScala.toSet
  }

  def checkVertex(node: Node): Unit = {
    if (!graph.containsVertex(node)) graph.addVertex(node)
  }
  
  private def addEdge(node1: Node, node2: Node, edge: Edge): DepGraph[Node] = {
    checkVertex(node1)
    checkVertex(node2)
    graph.addEdge(node1,node2,edge)
    this
  }

  override def addEdge(node1: Node, posNeg: PosNeg, node2: Node): DepGraph[Node] = {
    addEdge(node1,node2,Edge(node1,posNeg,node2))
  }
  
  override def outEdges(node: Node): Either[String,Set[(PosNeg,Node)]] = {
    println("Node: " + node)
    if (graph.containsVertex(node)) {
      println(s"Graph constains $node") 
      val edges: Set[Edge] = graph.outgoingEdgesOf(node).asScala.toSet
      Right(edges.map(e => (e.posNeg,e.target)))
    } else {
      Left(s"Node $node is not in graph $graph") 
    }
  }

  private def containsNegEdge(g: DirectedSubgraph[Node,Edge]): Boolean = {
    g.edgeSet.asScala.exists(e => e.posNeg == Neg) 
  }
  
  override def negCycles: Set[Set[Node]] = {
     val scAlg: StrongConnectivityAlgorithm[Node, Edge] = 
       new KosarajuStrongConnectivityInspector(graph)
     val sccSubgraphs =
       scAlg.stronglyConnectedSubgraphs().asScala.toSet
    sccSubgraphs.filter(containsNegEdge(_)).map(getNodes(_))
  }
  
  private def getNodes(g: DirectedSubgraph[Node,Edge]): Set[Node] = {
    g.vertexSet.asScala.toSet
  }

  def showPosNeg(pn:PosNeg): String = {
    pn match {
      case Pos => "-(+)->"
      case Neg => "-(-)->"
    }
  }

  def showEdges(showNode:Node => String): String = {
    val str = new StringBuilder
    for (edge <- graph.edgeSet.asScala) {
      str ++= s"${showNode(edge.source)} ${showPosNeg(edge.posNeg)} ${showNode(edge.target)}\n"
    }
    str.toString
  }
  
}

