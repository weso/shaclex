package es.weso.shapeMaps

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
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

  override def relativize(maybeBase: Option[IRI]): QueryShapeMap = maybeBase match {
    case None => this
    case Some(base) => QueryShapeMap(
      associations.map(_.relativize(base)),
      nodesPrefixMap,
      shapesPrefixMap)
  }
}

object QueryShapeMap {
  implicit val encodeShapeMap: Encoder[QueryShapeMap] = new Encoder[QueryShapeMap] {
    final def apply(a: QueryShapeMap): Json = a.associations.asJson
  }
}

