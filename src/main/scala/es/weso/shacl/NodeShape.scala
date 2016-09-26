package es.weso.shacl

import es.weso.rdf.nodes._
import cats._

case class NodeShape(node: RDFNode, shape: Shape) {
  
 implicit val nodeShapeShow = new Show[NodeShape] {
  def show(ns: NodeShape) = s"[$node,${shape.showId}]"
 }
 
 override def toString = Show[NodeShape].show(this)

}
