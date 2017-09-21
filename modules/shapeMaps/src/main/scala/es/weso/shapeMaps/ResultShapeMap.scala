package es.weso.shapeMaps

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.RDFNode

case class ResultShapeMap(
  map: Map[RDFNode, Map[ShapeMapLabel, Info]],
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap) extends ShapeMap {
  def addNodeAssociations(node: RDFNode, mapLabels: Map[ShapeMapLabel, Info]): ResultShapeMap = {
    this.copy(map = this.map.updated(node, map(node) ++ mapLabels))
  }

  val associations: List[Association] = map.toList.flatMap {
    case (node, labelsMap) => {
      labelsMap.toList.map {
        case (shapeLabel, info) => {
          Association(RDFNodeSelector(node), shapeLabel, info)
        }
      }
    }
  }

  override def addAssociation(a: Association): Either[String, ResultShapeMap] = {
    a.nodeSelector match {
      case RDFNodeSelector(node) => {
        map.get(node) match {
          case None => Right(this.copy(map = map.updated(node, Map(a.shapeLabel -> a.info))))
          case Some(labelsMap) => {
            labelsMap.get(a.shapeLabel) match {
              case None => Right(this.copy(map = map.updated(node, labelsMap.updated(a.shapeLabel, a.info))))
              case Some(info) =>
                if (info.status == a.info.status) Right(this)
                else Left(s"Cannot add association with contradictory status: Association: ${a}, Labels map: ${labelsMap}")
            }
          }
        }
      }
      case _ => Left(s"Only RDFNode's can be added as associations to fixedShapeMaps. Value = ${a.nodeSelector}")
    }
  }

}

object ResultShapeMap {
  def empty = ResultShapeMap(Map(), PrefixMap.empty, PrefixMap.empty)
}

