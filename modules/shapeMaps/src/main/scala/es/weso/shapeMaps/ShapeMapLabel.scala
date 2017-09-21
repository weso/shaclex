package es.weso.shapeMaps
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{ BNodeId, IRI }
import io.circe.{ Encoder, Json }

abstract class ShapeMapLabel {
  def isStart: Boolean = this match {
    case Start => true
    case _ => false
  }
}

case class IRILabel(iri: IRI) extends ShapeMapLabel
case class BNodeLabel(bnode: BNodeId) extends ShapeMapLabel
case object Start extends ShapeMapLabel

object ShapeMapLabel {

  implicit val encodeShapeMapLabel: Encoder[ShapeMapLabel] = new Encoder[ShapeMapLabel] {
    final def apply(label: ShapeMapLabel): Json = {
      label match {
        case Start => Json.fromString("start")
        case IRILabel(iri) => Json.fromString(iri.toString)
        case BNodeLabel(bnode) => Json.fromString(bnode.toString)
      }
    }
  }

}