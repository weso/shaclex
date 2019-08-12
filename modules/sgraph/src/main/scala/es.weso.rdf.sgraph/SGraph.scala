package es.weso.rdf.sgraph

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import io.circe.Json

/**
* Representation of RDF graphs as simple graphs
 * It is used to serialize RDF graphs to DOT or JSON
 * @param rdfNodeIdMap
 * @param edges
 */
case class SGraph(rdfNodeIdMap: Map[RDFNode, Node],
                  edges: List[Edge]
                 ) {

  def addNode(node: RDFNode, pm: PrefixMap): (SGraph, Node) = rdfNodeIdMap.get(node) match {
    case None => {
      val id = "N" + nextId
      val label = pm.qualify(node)
      println(s"Label: $label, node: $node\nPrefixMap: $pm")
      val n = Node(id, label, node, pm)
      val newMap = rdfNodeIdMap.updated(node, n)
      (this.copy(rdfNodeIdMap = newMap), n)
    }
    case Some(n) => (this, n)
  }

  private def nextId = rdfNodeIdMap.size

  def addEdge(edge: Edge): SGraph = {
    this.copy(edges = edge +: edges)
  }

  def toDot(prefs: RDFDotPreferences): String = {
    val sb = new StringBuilder
    sb.append("digraph {\n")
    rdfNodeIdMap.values.foreach { node =>
      sb.append(node.toDot(prefs) + "\n")
    }
    edges.foreach { edge =>
      sb.append(edge.toDot(prefs) + "\n")
    }
    sb.append("}")
    sb.toString
  }

  def toJson: Json = Json.Null /* rdfNodeIdMap.values.foreach { node =>

  }*/
}

object SGraph {
  def empty: SGraph = SGraph(Map(),List())
}