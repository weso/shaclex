package es.weso.collection

import scala.collection.SortedMap

trait Graph[Node,Edge] {

  /**
   * nodes of a graph
   */
  def nodes: Set[Node]
  
  def addNode(node: Node): Graph[Node,Edge]
  
  def empty: Graph[Node,Edge]

}