package es.weso.shex.validator

import es.weso.rdf.nodes._
import cats._

case class NodeShape(node: RDFNode, shape: ShapeType) {
  override def toString = NodeShape.nodeShapeShow.show(this)
}

object NodeShape {

  implicit val nodeShapeShow = new Show[NodeShape] {
    def show(ns: NodeShape) = s"[${ns.node},${ns.shape}]"

  }

}
