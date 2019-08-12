package es.weso.shapeMaps

import es.weso.rdf.nodes.{BNode, IRI, RDFNode}
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import cats._
import cats.implicits._

sealed abstract class ShapeMapLabel {
  def isStart: Boolean = this match {
    case Start => true
    case _ => false
  }
  def isBNodeLabel: Boolean = this match {
    case _: BNodeLabel => true
    case _ => false
  }

  def relativize(base: IRI): ShapeMapLabel = this match {
    case IRILabel(iri) => IRILabel(iri.relativizeIRI(base))
    case other => other
  }
}

case class IRILabel(iri: IRI) extends ShapeMapLabel
case class BNodeLabel(bnode: BNode) extends ShapeMapLabel
case object Start extends ShapeMapLabel

object ShapeMapLabel {

  implicit val encodeShapeMapLabel: Encoder[ShapeMapLabel] = new Encoder[ShapeMapLabel] {
    final def apply(label: ShapeMapLabel): Json = {
      label match {
        case Start => Json.fromString(IRI("http://www.w3.org/ns/shex#Start").toString)
        case IRILabel(iri) => Json.fromString(iri.toString)
        case BNodeLabel(bnode) => Json.fromString(bnode.toString)
      }
    }
  }

  implicit val showShapeMapLabel: Show[ShapeMapLabel] = new Show[ShapeMapLabel] {
    def show(s: ShapeMapLabel): String = s match {
      case IRILabel(iri) => iri.show
      case BNodeLabel(bn) => bn.show
      case Start => "start"
    }
  }

  implicit val decodeShapeMapLabel: Decoder[ShapeMapLabel] = Decoder.instance { c =>
    c.as[String].flatMap(s => RDFNode.fromString(s).fold(
      s => Left(DecodingFailure(s, Nil)),
      node => node match {
        case iri: IRI => Right(IRILabel(iri))
        case bnode: BNode => Right(BNodeLabel(bnode))
        case _ => Left(DecodingFailure(s"Cannot parse shapeMapLabel $node", Nil))
      }))
  }

}