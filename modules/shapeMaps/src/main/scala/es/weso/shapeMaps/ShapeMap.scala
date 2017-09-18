package es.weso.shapeMaps

import es.weso.rdf.nodes._
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.{ PrefixMap, RDFReader }
import io.circe.JsonObject._
import io.circe._
import io.circe.syntax._
import ShapeMap._

abstract class ShapeMap {
  val associations: List[Association]

  def addAssociation(a: Association): Either[String, ShapeMap]

  def toJson: Json = {
    this.asJson
  }

}

object ShapeMap {

  def empty: ShapeMap = FixedShapeMap.empty

  def parse(str: String, nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): Either[String, QueryShapeMap] = {
    Parser.parse(str, nodesPrefixMap, shapesPrefixMap)
  }

  /**
   * Resolve triple patterns according to an RDF
   */
  def fixShapeMap(shapeMap: ShapeMap, rdf: RDFReader): Either[String, FixedShapeMap] = {
    val empty: Either[String, FixedShapeMap] = Right(FixedShapeMap.empty)

    def addNode(a: Association)(node: RDFNode, current: Either[String, FixedShapeMap]): Either[String, FixedShapeMap] = for {
      fixed <- current
      newShapeMap <- fixed.addAssociation(Association(RDFNodeSelector(node), a.shapeLabel))
    } yield newShapeMap

    def combine(a: Association, current: Either[String, FixedShapeMap]): Either[String, FixedShapeMap] = {
      a.nodeSelector match {
        case RDFNodeSelector(node) => for {
          sm <- current
          newSm <- sm.addAssociation(a)
        } yield newSm
        case TriplePattern(Focus, p, o) => o match {
          case WildCard => {
            val nodes = rdf.triplesWithPredicate(p).map(_.subj)
            nodes.foldRight(current)(addNode(a))
          }
          case NodePattern(obj) => {
            val nodes = rdf.triplesWithPredicateObject(p, obj).map(_.subj)
            nodes.foldRight(current)(addNode(a))
          }
          case Focus =>
            Left(s"FixShapeMap: Inconsistent triple pattern in node selector with two Focus: ${a.nodeSelector}")
        }
        case TriplePattern(s, p, Focus) => s match {
          case WildCard => {
            val nodes = rdf.triplesWithPredicate(p).map(_.obj)
            nodes.foldRight(current)(addNode(a))
          }
          case NodePattern(subj) => {
            val nodes = rdf.triplesWithPredicateObject(p, subj).map(_.obj)
            nodes.foldRight(current)(addNode(a))
          }
          case Focus =>
            Left(s"FixShapeMap: Inconsistent triple pattern in node selector with two Focus: ${a.nodeSelector}")
        }
        case _ => Left(s"FixShapeMap: Inconsistent triple pattern in node selector ${a.nodeSelector}")
      }
    }
    shapeMap.associations.foldRight(empty)(combine)
  }

  implicit val encodeShapeMap: Encoder[ShapeMap] = new Encoder[ShapeMap] {
    final def apply(a: ShapeMap): Json = {
      Json.fromJsonObject(JsonObject.empty)
    }
  }

}
