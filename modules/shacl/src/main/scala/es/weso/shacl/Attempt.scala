package es.weso.shacl

import es.weso.rdf.nodes._
import cats._
import cats.syntax.show._
import es.weso.rdf.path.SHACLPath
/**
 * Represents current validation attempt
 * It contains the node and a shape
 * It may contain a predicate, path or nothing
 */
case class Attempt(nodeShape: NodeShapePair, path: Option[SHACLPath]) {
  def node: RDFNode = nodeShape.node
  def shapeId: RDFNode = nodeShape.shape.id
}

