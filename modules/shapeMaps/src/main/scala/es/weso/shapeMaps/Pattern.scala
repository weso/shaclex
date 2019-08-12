package es.weso.shapeMaps

import es.weso.rdf.nodes.{IRI, RDFNode}
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed abstract class Pattern {
  def relativize(base: IRI): Pattern
}

case class NodePattern(node: RDFNode) extends Pattern {
  override def relativize(base: IRI): NodePattern =
    NodePattern(node.relativize(base))
}

case object WildCard extends Pattern {
  override def relativize(base: IRI) = this

}

case object Focus extends Pattern {
  override def relativize(base: IRI) = this
}

object Pattern {

  implicit val encodePattern: Encoder[Pattern] = new Encoder[Pattern] {
    final def apply(pattern: Pattern): Json = {
      pattern match {
        case Focus => Json.fromString("focus")
        case WildCard => Json.fromString("_")
        case NodePattern(node) => Json.fromString(node.getLexicalForm)
      }
    }
  }

  implicit val deodePattern: Decoder[Pattern] = Decoder.instance { c =>
    c.as[String].flatMap(_ match {
      case "focus" => Right(Focus)
      case "_" => Right(WildCard)
      case s => RDFNode.fromString(s).fold(
        s => Left(DecodingFailure(s, Nil)),
        node => Right(NodePattern(node)))
    })
  }
}
