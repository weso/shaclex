package es.weso.shapeMaps

import es.weso.rdf.nodes.RDFNode
import io.circe.{ Encoder, Json }

sealed abstract class Pattern
case class NodePattern(node: RDFNode) extends Pattern
case object WildCard extends Pattern
case object Focus extends Pattern

object Pattern {
  implicit val encodePattern: Encoder[Pattern] = new Encoder[Pattern] {
    final def apply(pattern: Pattern): Json = {
      pattern match {
        case Focus => Json.fromString("focus")
        case WildCard => Json.fromString("_")
        case NodePattern(node) => Json.fromString(node.toString)
      }
    }
  }
}