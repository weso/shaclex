package es.weso.shapeMaps

import cats.Show
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.RDFNode

case class FixedShapeMap(
  shapeMap: Map[RDFNode, Map[ShapeMapLabel, Info]],
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap) extends ShapeMap {

  val associations: List[Association] = shapeMap.toList.flatMap {
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
        shapeMap.get(node) match {
          case None => Right(this.copy(shapeMap = shapeMap.updated(node, Map(a.shapeLabel -> a.info))))
          case Some(labelsMap) => {
            labelsMap.get(a.shapeLabel) match {
              case None => Right(this.copy(shapeMap = shapeMap.updated(node, labelsMap.updated(a.shapeLabel, a.info))))
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
  def empty = FixedShapeMap(Map(), PrefixMap.empty, PrefixMap.empty)

}