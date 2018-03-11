package es.weso.shapeMaps

import io.circe._
import io.circe.syntax._
import NodeSelector._
import ShapeMapLabel._
import es.weso.json.DecoderUtils._
import es.weso.rdf.nodes.{ BNode, IRI, RDFNode }

case class Association(node: NodeSelector, shape: ShapeMapLabel, info: Info = Info()) {

  def toJson: Json = {
    this.asJson
  }

}

object Association {

  implicit val encodeAssociation: Encoder[Association] = new Encoder[Association] {
    final def apply(a: Association): Json = {
      val obj = JsonObject.empty.
        add("node", a.node.asJson).
        add("shape", a.shape.asJson).
        add("status", a.info.status.asJson).
        add("appInfo", a.info.appInfo)
      Json.fromJsonObject(a.info.reason match {
        case None => obj
        case Some(reason) => obj.add("reason", reason.asJson)
      })
    }
  }

  implicit val decodeStatus: Decoder[Status] = Decoder.instance { c =>
    c.as[String].flatMap(_.toLowerCase match {
      case "conformant" => Right(Conformant)
      case "nonconformant" => Right(NonConformant)
      case s => Left(DecodingFailure(s"Error parsing status. Unsupported value $s", Nil))
    })
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

  implicit val decodeAssociation: Decoder[Association] = new Decoder[Association] {
    final def apply(c: HCursor): Decoder.Result[Association] = for {
      node <- c.downField("node").as[NodeSelector]
      shape <- c.downField("shape").as[ShapeMapLabel]
      status <- c.downField("status").as[Status]
      reason <- optFieldDecode[String](c, "reason")
      appInfo <- c.downField("appInfo").as[Json]
    } yield Association(node, shape, Info(status, reason, appInfo))
  }

}
