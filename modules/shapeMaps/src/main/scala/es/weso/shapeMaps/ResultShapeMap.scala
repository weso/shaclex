package es.weso.shapeMaps

import es.weso.rdf.nodes.RDFNode

case class ResultShapeMap(override val map: Map[RDFNode, Map[ShapeMapLabel, Info]]) extends FixedShapeMap(map) {
  def addNodeAssociations(node: RDFNode, mapLabels: Map[ShapeMapLabel, Info]): ResultShapeMap = {
    ResultShapeMap(map.updated(node, map(node) ++ mapLabels))
  }
}

object ResultShapeMap {
  def empty = ResultShapeMap(Map())
}

