package es.weso.shacl

import es.weso.rdf.nodes._
import cats._, data._
import cats.implicits._

case class NodeShapePair(node: RDFNode, shape: ShapeRef) {

  override def toString = NodeShapePair.nodeShapeShow.show(this)

}

object NodeShapePair {
  implicit val nodeShapeShow = new Show[NodeShapePair] {
    def show(ns: NodeShapePair) = s"[${ns.node},${ns.shape.showId}]"
  }
}
