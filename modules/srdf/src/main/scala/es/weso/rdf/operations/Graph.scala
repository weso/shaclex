package es.weso.rdf.operations
import es.weso.rdf.nodes._
import es.weso.rdf._
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.triples.RDFTriple

object Graph {

  /**
  * traverse the outgoing nodes of a node
    * It currently only takes into account the ourgoing arcs
    * @param node starting node
    * @param rdf RDFReader
    * @return list of visited nodes
    */
  def traverse(node: RDFNode, rdf: RDFReader): List[RDFNode] = {
    def outgoing(node: RDFNode): List[RDFNode] =
      rdf.triplesWithSubject(node).map(_.obj).toList

    def outgoingNotVisited(node: RDFNode,
                           visited: List[RDFNode]
                          ): List[RDFNode] =
      outgoing(node).filter(x => x != node && !visited.contains(x))

    @annotation.tailrec
    def loop(stack: List[RDFNode],
             visited: List[RDFNode]
            ): List[RDFNode] = stack match {
      case Nil => visited
      case head :: tail => {
        loop(outgoingNotVisited(head, visited) ++ tail, head :: visited)
      }
    }
    loop(List(node),List())
  }

  def traverseWithArcs(node: RDFNode, rdf: RDFReader): (List[RDFNode], List[RDFTriple]) = {
    val nodes = traverse(node,rdf)
    val triples = nodes.map(rdf.triplesWithSubject(_)).flatten
    (nodes, triples)
  }

}