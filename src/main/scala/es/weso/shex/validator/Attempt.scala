package es.weso.shex.validator

import es.weso.rdf.nodes._

/**
 * Represents current validation attempt
 * It contains the node and a shape
 * It may contain a predicate, path or nothing
 */
case class Attempt(nodeShape: NodeShape, path: Option[IRI]) {
  def node = nodeShape.node

  def predicate: Option[IRI] = path

}

