package es.weso.depgraphs

import org.jgrapht.Graph
import org.jgrapht.graph._
import org.jgrapht.alg._

import collection.JavaConverters._
import cats.implicits._
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm

case class DepGraphJGraphT[Node]() extends DepGraph[Node] {

  case class Edge(source: Node, posNeg: PosNeg, target: Node)

  val graph: Graph[Node, Edge] =
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

  private def removeEdge(node1: Node, node2:Node): Unit = {
    graph.removeEdge(node1,node2)
  }

  override def addEdge(node1: Node, posNeg: PosNeg, node2: Node): DepGraph[Node] = {
    checkVertex(node1)
    checkVertex(node2)
    this.edgeBetween(node1,node2) match {
      case Some(pn) => {
        removeEdge(node1,node2)
        addEdge(node1, node2, Edge(node1, pn.combine(posNeg), node2))
      }
      case None => addEdge(node1, node2, Edge(node1, posNeg, node2))
    }
  }

  override def edgeBetween(node1: Node, node2: Node): Option[PosNeg] = {
    val outEdges = graph.edgesOf(node1).asScala.toSet
    outEdges.collect{ case e: Edge if e.target == node2 => e.posNeg }.headOption
  }

  override def outEdges(node: Node): Either[String, Set[(PosNeg, Node)]] = {
    if (graph.containsVertex(node)) {
      val edges: Set[Edge] = graph.outgoingEdgesOf(node).asScala.toSet
      Right(edges.map(e => (e.posNeg, e.target)))
    } else {
      Left(s"Node $node is not in graph $graph")
    }
  }

  private def containsNegEdge(g: Graph[Node, Edge]): Boolean = {
    g.edgeSet.asScala.exists(e => e.posNeg == Neg || e.posNeg == Both)
  }

  override def negCycles: Set[Set[(Node,Node)]] = {
    val scAlg: StrongConnectivityAlgorithm[Node, Edge] =
      new KosarajuStrongConnectivityInspector(graph)
    val sccSubgraphs =
      scAlg.getStronglyConnectedComponents.asScala.toSet  // stronglyConnectedSubgraphs().asScala.toSet
    sccSubgraphs.filter(containsNegEdge(_)).map(getEdges(_))
  }

  private def getEdges(g: Graph[Node, Edge]): Set[(Node,Node)] = {
    val es = g.edgeSet.asScala.toSet
    // println(s"getEdges($g)=$es")
    es.map(e => (e.source,e.target))
  }

/*  private def getNodes(g: Graph[Node, Edge]): Set[Node] = {
    val ns = g.vertexSet.asScala.toSet
    // println(s"getNodes($g)=$ns")
    ns
  } */

  def showPosNeg(pn: PosNeg): String = {
    pn match {
      case Pos => "-(+)->"
      case Neg => "-(-)->"
      case Both => "-(-/+)->"
    }
  }

  def showEdges(showNode: Node => String): String = {
    val str = new StringBuilder
    for (edge <- graph.edgeSet.asScala) {
      str ++= s"${showNode(edge.source)} ${showPosNeg(edge.posNeg)} ${showNode(edge.target)}\n"
    }
    str.toString
  }

  type ES[A] = Either[String,A]

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
      rs.sequence[ES,Unit].map(_ => ())
    } else
      Left(s"Set of nodes is different. Nodes1 = $nodes1, nodes2 = $nodes2")
  }
}

