package es.weso.shapeMaps

import cats.Show
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.RDFNode
import io.circe._
import io.circe.syntax._

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
    a.node match {
      case RDFNodeSelector(node) => {
        shapeMap.get(node) match {
          case None => Right(this.copy(shapeMap = shapeMap.updated(node, Map(a.shape -> a.info))))
          case Some(labelsMap) => {
            labelsMap.get(a.shape) match {
              case None => Right(this.copy(shapeMap = shapeMap.updated(node, labelsMap.updated(a.shape, a.info))))
              case Some(info) =>
                if (info.status == a.info.status) Right(this)
                else Left(s"Cannot add association with contradictory status: Association: ${a}, Labels map: ${labelsMap}")
            }
          }
        }
      }
      case _ => Left(s"Only RDFNode's can be added as associations to fixedShapeMaps. Value = ${a.node}")
    }
  }
}

object FixedShapeMap {
  def empty = FixedShapeMap(Map(), PrefixMap.empty, PrefixMap.empty)

  implicit val encodeShapeMap: Encoder[ResultShapeMap] = new Encoder[ResultShapeMap] {
    final def apply(a: ResultShapeMap): Json = a.associations.asJson
  }
}