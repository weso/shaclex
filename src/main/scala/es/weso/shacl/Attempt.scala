package es.weso.shacl

import es.weso.rdf.nodes._
import cats._
import cats.syntax.show._
/**
 * Represents current validation attempt
 * It contains the node and a shape 
 * It may contain a predicate, path or nothing
 */
case class Attempt(nodeShape: NodeShape, path: Option[IRI]) {
  def node = nodeShape.node
  
  def shapeIRI = nodeShape.shape.id
  
  def predicate: Option[IRI] = path 

}

case class NodeShape(node: RDFNode, shape: Shape) {
  
 implicit val nodeShapeShow = new Show[NodeShape] {
  def show(ns: NodeShape) = s"[$node,${shape.showId}]"
 }
 
 override def toString = Show[NodeShape].show(this)

}