package es.weso.shapeMaps

import io.circe.Json

/**
 * Input shape map also known as Query shape map
 * @param associations
 */
case class QueryShapeMap(associations: List[Association]) extends ShapeMap {

  override def addAssociation(a: Association): Either[String, QueryShapeMap] = Right(QueryShapeMap(a +: associations))

}

