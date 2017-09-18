package es.weso.shapeMaps

import es.weso.rdf.nodes.RDFNode

case class FixedShapeMap(map: Map[RDFNode, Map[ShapeMapLabel, Info]]) extends ShapeMap {

  val associations: List[Association] = map.toList.flatMap {
    case (node, labelsMap) => {
      labelsMap.toList.map {
        case (shapeLabel, info) => {
          Association(RDFNodeSelector(node), shapeLabel, info)
        }
      }
    }
  }

  override def addAssociation(a: Association): Either[String, FixedShapeMap] = {
    a.nodeSelector match {
      case RDFNodeSelector(node) => {
        map.get(node) match {
          case None => Right(FixedShapeMap(map.updated(node, Map(a.shapeLabel -> a.info))))
          case Some(labelsMap) => {
            labelsMap.get(a.shapeLabel) match {
              case None => Right(FixedShapeMap(map.updated(node, labelsMap.updated(a.shapeLabel, a.info))))
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

object FixedShapeMap {
  def empty = FixedShapeMap(Map())
}
