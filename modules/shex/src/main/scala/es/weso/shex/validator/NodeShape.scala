package es.weso.shex.validator

import es.weso.rdf.nodes._
import cats._
import es.weso.rdf.PrefixMap

case class NodeShape(node: RDFNode, shape: ShapeType) {
  override def toString = NodeShape.nodeShapeShow.show(this)

  def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    s"${nodesPrefixMap.qualify(node)}@${shape.label.map(label => shapesPrefixMap.qualify(label.toRDFNode))}"
  }
}

object NodeShape {

  implicit val nodeShapeShow = new Show[NodeShape] {
    def show(ns: NodeShape) = s"[${ns.node},${ns.shape.label.map(label => label.toRDFNode.toString).getOrElse("Anonymous shape")}]"

  }

}
