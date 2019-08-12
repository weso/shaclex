package es.weso.shapeMaps

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.utils.MapUtils.cnvMapMap
import io.circe._
import io.circe.syntax._

case class FixedShapeMap(
  shapeMap: Map[RDFNode, Map[ShapeMapLabel, Info]],
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap) extends ShapeMap {

  lazy val flatten: List[(RDFNode, ShapeMapLabel, Status)] =
    shapeMap.toList.map(p => p._2.toList.map(q => (p._1, q._1, q._2.status))).flatten

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

  def addNodesPrefixMap(pm: PrefixMap): FixedShapeMap =
    this.copy(nodesPrefixMap = pm)

  def addShapesPrefixMap(pm: PrefixMap): FixedShapeMap =
    this.copy(shapesPrefixMap = pm)

  private def cnvFixedShapeMap(cnvNode: RDFNode => RDFNode,
                           cnvLabel: ShapeMapLabel => ShapeMapLabel
                          ): FixedShapeMap =
    FixedShapeMap(
      cnvMapMap(shapeMap, cnvNode, cnvLabel, identity[Info]),
      nodesPrefixMap,
      shapesPrefixMap
    )

  override def relativize(maybeBase: Option[IRI]): FixedShapeMap = maybeBase match {
    case None => this
    case Some(base) => cnvFixedShapeMap(_.relativize(base), _.relativize(base))
  }

}

object FixedShapeMap {
  def empty = FixedShapeMap(Map(), PrefixMap.empty, PrefixMap.empty)

  implicit val encodeShapeMap: Encoder[ResultShapeMap] = new Encoder[ResultShapeMap] {
    final def apply(a: ResultShapeMap): Json = a.associations.asJson
  }
}