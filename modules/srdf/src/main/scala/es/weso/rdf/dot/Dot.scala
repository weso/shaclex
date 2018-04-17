package es.weso.rdf.dot

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._


case class Node(id: String, label: String, rdfNode: RDFNode, pm: PrefixMap) {

  def labelLiteral(l: Literal): String = l match {
    case s: StringLiteral => s.getLexicalForm
    case i: IntegerLiteral => i.int.toString
    case l: LangLiteral => l.lexicalForm + "@" + l.lang.lang
    case dt: DatatypeLiteral => l.getLexicalForm + "^^" + pm.qualify(l.dataType)
    case _ => l.getLexicalForm
  }

  override def toString: String = rdfNode match {
    case i: IRI => s"""node [shape=ellipse, style=solid, color=black, label="$label", href="${i.str}"] $id ;"""
    case l: Literal => s"""node[shape=rectangle, style=filled, color=yellow, label="${labelLiteral(l)}"] $id ;"""
    case b: BNode => s"""node[shape=circle, style=filled, color=gray] $id ;"""
  }
}

case class Edge(n1: Node, n2: Node, label: String, href: String) {
  override def toString: String = {
    s"""${n1.id} -> ${n2.id} [label = "$label", href = "$href"] ;"""
  }
}

case class DotGraph(
                     rdfNodeIdMap: Map[RDFNode, Node],
                     edges: List[Edge]
                   ) {

  val IRIStyle = "solid"
  val LiteralStyle = "filled"
  val BNodeStyle = "filled"

  val IRIColor = "white"
  val LiteralColor = "yellow"
  val BNodeColor = "gray"

  def addNode(node: RDFNode, pm: PrefixMap): (DotGraph, Node) = rdfNodeIdMap.get(node) match {
    case None => {
      val id = "N" + nextId
      val label = pm.qualify(node)
      val (href,style, color) = node match {
        case i: IRI => (i.str, IRIStyle, IRIColor)
        case l: Literal => (l.getLexicalForm, LiteralStyle, LiteralColor)
        case _: BNode => (id, BNodeStyle, BNodeColor)
      }
      val n = Node(id, label, node, pm)
      val newMap = rdfNodeIdMap.updated(node, n)
      (this.copy(rdfNodeIdMap = newMap), n)
    }
    case Some(n) => (this, n)
  }

  def nextId = rdfNodeIdMap.size

  def addEdge(edge: Edge): DotGraph = {
    this.copy(edges = edge +: edges)
  }

  override def toString = {
    val sb = new StringBuilder
    sb.append("digraph {\n")
    rdfNodeIdMap.values.foreach { node =>
      sb.append(node + "\n")
    }
    edges.foreach { edge =>
      sb.append(edge + "\n")
    }
    sb.append("}")
    sb.toString
  }
}

object DotGraph {
  def empty: DotGraph = DotGraph(Map(),List())
}