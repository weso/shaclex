package es.weso.depgraphs

import org.jgrapht.DirectedGraph
import org.jgrapht.graph.DefaultDirectedGraph
import org.jgrapht.graph.DirectedSubgraph

import org.jgrapht.alg.KosarajuStrongConnectivityInspector
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm
import collection.JavaConverters._
import cats.implicits._

case class DepGraphJGraphT[Node]() extends DepGraph[Node] {

  case class Edge(source: Node, posNeg: PosNeg, target: Node)

  val graph: DirectedGraph[Node, Edge] =
    new DefaultDirectedGraph[Node, Edge](classOf[Edge])

  /**
    * Removes all edges
    * @return <tt>true</tt> if the graph changed
    */
  def removeAllEdges(): Boolean = {
    val edges: java.util.Set[Edge] = graph.edgeSet
    graph.removeAllEdges(edges)
  }

  override def empty: DepGraph[Node] = {
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

  /**
    * Checks if a node is in a graph and adds it if it isn't
    * @param node
    * @return <tt>true</tt> if it added the node
    */
  def checkVertex(node: Node): Boolean = {
    // if (!graph.containsVertex(node))
    graph.addVertex(node)
  }

  private def addEdge(node1: Node, node2: Node, edge: Edge): DepGraph[Node] = {
    checkVertex(node1)
    checkVertex(node2)
    graph.addEdge(node1, node2, edge)
    this
  }

  override def addEdge(node1: Node, posNeg: PosNeg, node2: Node): DepGraph[Node] = {
    addEdge(node1, node2, Edge(node1, posNeg, node2))
  }

  override def outEdges(node: Node): Either[String, Set[(PosNeg, Node)]] = {
    if (graph.containsVertex(node)) {
      val edges: Set[Edge] = graph.outgoingEdgesOf(node).asScala.toSet
      Right(edges.map(e => (e.posNeg, e.target)))
    } else {
      Left(s"Node $node is not in graph $graph")
    }
  }

  private def containsNegEdge(g: DirectedSubgraph[Node, Edge]): Boolean = {
    g.edgeSet.asScala.exists(e => e.posNeg == Neg)
  }

  override def negCycles: Set[Set[Node]] = {
    val scAlg: StrongConnectivityAlgorithm[Node, Edge] =
      new KosarajuStrongConnectivityInspector(graph)
    val sccSubgraphs =
      scAlg.stronglyConnectedSubgraphs().asScala.toSet
    sccSubgraphs.filter(containsNegEdge(_)).map(getNodes(_))
  }

  private def getNodes(g: DirectedSubgraph[Node, Edge]): Set[Node] = {
    g.vertexSet.asScala.toSet
  }

  def showPosNeg(pn: PosNeg): String = {
    pn match {
      case Pos => "-(+)->"
      case Neg => "-(-)->"
    }
  }

  def showEdges(showNode: Node => String): String = {
    val str = new StringBuilder
    for (edge <- graph.edgeSet.asScala) {
      str ++= s"${showNode(edge.source)} ${showPosNeg(edge.posNeg)} ${showNode(edge.target)}\n"
    }
    str.toString
  }

  override def isomorphicWith(other: DepGraph[Node]): Either[String, Unit] = {
    val nodes1 = this.nodes
    val nodes2 = other.nodes
    if (nodes1 == nodes2) {
      val rs: List[Either[String, Unit]] = nodes1.map(n => outEdges(n) match {
        case Left(msg) => Left(s"Cannot find outEdges of $n in graph1: $msg")
        case Right(es1) => other.outEdges(n) match {
          case Left(msg) => Left(s"Cannot find outEdges of $n in graph2. Error: $msg")
          case Right(es2) => if (es1 == es2) {
            Right(())
          } else {
            Left(s"Outedges of $n are different. Graph1 = $es1, Graph2 = $es2")
          }
        }
      }).toList
      rs.sequence.map(_ => ())
    } else
      Left(s"Set of nodes is different. Nodes1 = $nodes1, nodes2 = $nodes2")
  }
}

