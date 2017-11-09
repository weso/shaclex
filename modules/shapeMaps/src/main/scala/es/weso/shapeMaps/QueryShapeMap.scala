package es.weso.shapeMaps

import es.weso.rdf.PrefixMap
import io.circe._
import io.circe.syntax._

/**
 * Input shape map also known as Query shape map
 * @param associations
 */
case class QueryShapeMap(
  associations: List[Association],
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap) extends ShapeMap {

  override def addAssociation(a: Association): Either[String, QueryShapeMap] =
    Right(this.copy(associations = a +: associations))

}

object QueryShapeMap {
  implicit val encodeShapeMap: Encoder[QueryShapeMap] = new Encoder[QueryShapeMap] {
    final def apply(a: QueryShapeMap): Json = a.associations.asJson
  }
}

