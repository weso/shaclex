package es.weso.schema
import es.weso.rdf.nodes._
import es.weso.rdf.PrefixMap
import io.circe.JsonObject._
import io.circe._
import io.circe.syntax._
import cats._
import cats.data._
import cats.syntax.either._
import cats.instances.either._
import InfoNode._
import es.weso.json.DecoderUtils._
import es.weso.rdf.nodes.IRI.parseIRI
import io.circe.Decoder._

case class Solution(
  nodes: Map[RDFNode, InfoNode],
  nodeMap: PrefixMap,
  schemaMap: PrefixMap) {

  override def toString: String = show

  def hasShapes(node: RDFNode): Seq[SchemaLabel] = {
    nodes.get(node) match {
      case Some(info) => info.hasShapes.map(_._1).toSeq
      case None => Seq()
    }
  }

  def show: String = {
    val sb = new StringBuilder
    sb ++= "Solution\n"
    if (nodes.isEmpty) {
      sb ++= "No nodes in solution"
    } else
      for (pair <- nodes.toSeq) {
        val (node, info) = pair
        sb ++= (nodeMap.qualify(node) + " " + info.show + "\n")
      }
    sb.toString
  }

  def toJson: Json = {
    val jsonMap: Json = Json.fromJsonObject(JsonObject.fromMap(
      nodes.map { case (node, info) => (node.getLexicalForm, info.asJson) }))
    Json.fromJsonObject(
      singleton("type", Json.fromString("Solution"))
        .add("solution", jsonMap))
  }

  def isEmpty: Boolean = {
    nodes.isEmpty
  }

}

object Solution {
  implicit val showSolution = new Show[Solution] {
    override def show(s: Solution): String = {
      s.show
    }
  }

  implicit val encodeSolution: Encoder[Solution] = new Encoder[Solution] {
    final def apply(a: Solution): Json =
      a.toJson
  }

  implicit val decodeSolution: Decoder[Solution] = new Decoder[Solution] {

    val keyDecoderRDFNode: KeyDecoder[RDFNode] =
      KeyDecoder.instance { str => parseIRI(str).toOption }

    val decoderInfoNode: Decoder[InfoNode] =
      InfoNode.decodeInfoNode

    final def apply(c: HCursor): Decoder.Result[Solution] = for {
      _ <- fixedFieldValue(c, "type", "Solution")
      solutionMap <- mapDecoder(c.downField("solution"))(keyDecoderRDFNode, decoderInfoNode)
    } yield Solution(solutionMap, PrefixMap.empty, PrefixMap.empty)

  }

}
