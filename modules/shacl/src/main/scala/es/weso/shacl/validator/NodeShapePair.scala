package es.weso.shacl.validator

import cats._
import es.weso.rdf.nodes._
import es.weso.shacl.ShapeRef

case class NodeShapePair(node: RDFNode, shape: ShapeRef) {

  override def toString = NodeShapePair.nodeShapeShow.show(this)

}

object NodeShapePair {
  implicit val nodeShapeShow = new Show[NodeShapePair] {
    def show(ns: NodeShapePair) = s"[${ns.node},${ns.shape.showId}]"
  }
}
