package es.weso.shapeMaps

import cats.Show
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{ IRI, RDFNode }
import es.weso.rdf.path.SHACLPath
import io.circe.{ Encoder, Json, JsonObject }
import io.circe.syntax._

abstract class NodeSelector {
  def toJson: Json = this match {
    case RDFNodeSelector(node) => Json.fromString(node.getLexicalForm)
    case TriplePattern(subj, predicate, obj) => {
      println("Not implemented TriplePattern toJson yet")
      ???
    }
  }
}
case class RDFNodeSelector(node: RDFNode) extends NodeSelector
case class TriplePattern(subjectPattern: Pattern, path: SHACLPath, objectPattern: Pattern) extends NodeSelector

object NodeSelector {
  implicit val encodeNodeSelector: Encoder[NodeSelector] = new Encoder[NodeSelector] {
    final def apply(nodeSelector: NodeSelector): Json = {
      nodeSelector match {
        case RDFNodeSelector(node) => Json.fromString(node.getLexicalForm)
        case TriplePattern(subj, pred, obj) => {
          Json.fromJsonObject(JsonObject.empty.
            add("subject", subj.asJson).
            add("predicate", Json.fromString(pred.toString)).
            add("object", obj.asJson))
        }
      }
    }
  }

}