package es.weso.shextest.manifest
import es.weso.rdf.nodes.RDFNode
import es.weso.shapeMaps._
import es.weso.shapeMaps.ShapeMapLabel._
import io.circe._
import io.circe.parser._
import io.circe.syntax._

case class ShapeResult(shapeMapLabel: ShapeMapLabel, value: Boolean)

object ShapeResult {
  implicit val shapeResultDecoder: Decoder[ShapeResult] = Decoder.instance { c => {
    for {
      shape   <- c.downField("shape").as[ShapeMapLabel]
      value <- c.downField("result").as[Boolean]
    } yield ShapeResult(shape,value)
   }
  }

  implicit val shapeResultEncoder: Encoder[ShapeResult] = Encoder.instance { r =>
    Json.fromJsonObject(
      JsonObject.empty.
        add("shape", r.shapeMapLabel.asJson)
        add("result", r.value.asJson)
    )
  }

}

case class JsonResult(rmap: Map[RDFNode,List[ShapeResult]]) {
  def compare(resultMap: ResultShapeMap): Boolean = {
    def check(node: RDFNode, label: ShapeMapLabel, expected: Boolean): Boolean = {
      val isValid = resultMap.getConformantShapes(node).contains(label)
      isValid == expected
    }

    rmap.map {
      case (node,ls) => ls.map {
        case sr => check(node,sr.shapeMapLabel,sr.value)
      }
    }.flatten.forall(identity)
  }
}

object JsonResult {

  def fromJsonString(str: String): Either[String,JsonResult] = {
    decode[JsonResult](str).fold(e => Left(s"Error parsing string as JsonResult: $e"), Right(_))
  }

  implicit val rdfNodeDecoder: Decoder[RDFNode] = Decoder.instance { c => {
    c.as[String].flatMap(s => {
      RDFNode.fromString(s).fold(
        s => {
          Left(DecodingFailure(s, Nil))
        } ,
        node => Right(node))
    })
  }
  }

  implicit val rdfnodeKeyDecoder = new KeyDecoder[RDFNode] {
    override def apply(key: String): Option[RDFNode] = RDFNode.fromString(key).fold(_ => None, Some(_))
  }

  implicit val resultDecoder: Decoder[JsonResult] = Decoder.instance { c => {
    for {
      map <- c.as[Map[RDFNode,List[ShapeResult]]]
    } yield JsonResult(map)
  }
  }
  implicit val rdfNodeEncoder: Encoder[RDFNode] = Encoder.instance { r =>
    Json.fromString(r.toString)
  }
  implicit val rdfNodeKeyEncoder: KeyEncoder[RDFNode] = new KeyEncoder[RDFNode] {
    override def apply(node: RDFNode): String = node.toString
  }
  implicit val resultEncoder: Encoder[JsonResult] = Encoder.instance { r =>
    r.rmap.asJson
  }
}