package es.weso.shapeMaps

import cats.Show
import cats.syntax.show._
import es.weso.rdf.PrefixMap
import io.circe.Json

/**
 * Input shape map also known as Query shape map
 * @param associations
 */
case class QueryShapeMap(
  associations: List[Association],
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap) extends ShapeMap {

  override def addAssociation(a: Association): Either[String, QueryShapeMap] = Right(this.copy(associations = a +: associations))

}

