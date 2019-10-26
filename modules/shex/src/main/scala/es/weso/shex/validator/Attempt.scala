package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._

/**
 * Represents current validation attempt
 * It contains the node and a shape
 * It may contain a predicate, path or nothing
 */
case class Attempt(nodeShape: NodeShape, path: Option[IRI]) {
  def node = nodeShape.node
  def shape = nodeShape.shape

  def predicate: Option[IRI] = path

  def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    nodeShape.showQualified(nodesPrefixMap,shapesPrefixMap) ++ path.map(iri => s" Path: ${shapesPrefixMap.qualifyIRI(iri)}").getOrElse("")
  }

  override def toString: String = Attempt.showAttempt.show(this)
}

object Attempt {
  implicit def showAttempt = new Show[Attempt] {
    import NodeShape._
    override def show(t: Attempt): String = {
      s"Attempt: ${t.nodeShape.show}\npath: ${t.path.map(_.str).getOrElse("")}"
    }
  }

}

