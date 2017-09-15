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

case class InputShapeMap(associations: List[Association]) extends ShapeMap {

  override def addAssociation(a: Association): Either[String, InputShapeMap] = Right(InputShapeMap(a +: associations))

}

case class Association(
  nodeSelector: NodeSelector,
  shapeLabel: ShapeMapLabel,
  info: Info = Info()) {

  def toJson: Json = {
    this.asJson
  }

}

abstract sealed class Status
case object Conformant extends Status
case object NonConformant extends Status

abstract class ShapeMapLabel
case class IRILabel(iri: IRI) extends ShapeMapLabel
case object Start extends ShapeMapLabel

abstract class NodeSelector {
  def toJson: Json = this match {
    case RDFNodeSelector(node) => Json.fromString(node.toString)
    case TriplePattern(subj, predicate, obj) => ???
  }
}
case class RDFNodeSelector(node: RDFNode) extends NodeSelector
case class TriplePattern(subjectPattern: Pattern, predicate: IRI, objectPattern: Pattern) extends NodeSelector

sealed abstract class Pattern
case class NodePattern(node: RDFNode) extends Pattern
case object WildCard extends Pattern
case object Focus extends Pattern

case class Info(status: Status = Conformant, reason: Option[String] = None, appInfo: Json = Json.Null)

case class FixedShapeMap(map: Map[RDFNode, Map[ShapeMapLabel, Info]]) extends ShapeMap {

  val associations: List[Association] = map.toList.map {
    case (node, labelsMap) => {
      labelsMap.toList.map {
        case (shapeLabel, info) => {
          Association(RDFNodeSelector(node), shapeLabel, info)
        }
      }
    }
  }.flatten

  override def addAssociation(a: Association): Either[String, FixedShapeMap] = {
    a.nodeSelector match {
      case RDFNodeSelector(node) => {
        map.get(node) match {
          case None => Right(FixedShapeMap(map.updated(node, Map(a.shapeLabel -> a.info))))
          case Some(labelsMap) => {
            labelsMap.get(a.shapeLabel) match {
              case None => Right(FixedShapeMap(map.updated(node, labelsMap.updated(a.shapeLabel, a.info))))
              case Some(info) =>
                if (info.status == a.info.status) Right(this)
                else Left(s"Cannot add association with contradictory status: Association: ${a}, Labels map: ${labelsMap}")
            }
          }
        }
      }
      case _ => Left(s"Only RDFNode's can be added as associations to fixedShapeMaps. Value = ${a.nodeSelector}")
    }
  }

}

object FixedShapeMap {
  def empty = FixedShapeMap(Map())
}

object ShapeMap {

  def empty: ShapeMap = FixedShapeMap.empty

  def parse(str: String, nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): Either[String, InputShapeMap] = {
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

  implicit val encodeShapeMapLabel: Encoder[ShapeMapLabel] = new Encoder[ShapeMapLabel] {
    final def apply(label: ShapeMapLabel): Json = {
      label match {
        case Start => Json.fromString("start")
        case IRILabel(iri) => Json.fromString(iri.toString)
      }
    }
  }

  implicit val encodeNodeSelector: Encoder[NodeSelector] = new Encoder[NodeSelector] {
    final def apply(nodeSelector: NodeSelector): Json = {
      nodeSelector match {
        case RDFNodeSelector(node) => Json.fromString(node.toString)
        case TriplePattern(subj, pred, obj) => {
          Json.fromJsonObject(JsonObject.empty.
            add("subject", subj.asJson).
            add("predicate", Json.fromString(pred.toString)).
            add("object", obj.asJson))
        }
      }
    }
  }

  implicit val encodePattern: Encoder[Pattern] = new Encoder[Pattern] {
    final def apply(pattern: Pattern): Json = {
      pattern match {
        case Focus => Json.fromString("focus")
        case WildCard => Json.fromString("_")
        case NodePattern(node) => Json.fromString(node.toString)
      }
    }
  }

  implicit val encodeStatus: Encoder[Status] = new Encoder[Status] {
    final def apply(a: Status): Json = {
      a match {
        case Conformant => Json.fromString("conformant")
        case NonConformant => Json.fromString("nonconformant")
      }
    }
  }

  implicit val encodeAssociation: Encoder[Association] = new Encoder[Association] {
    final def apply(a: Association): Json = {
      Json.fromJsonObject(JsonObject.empty.
        add("nodeSelector", a.nodeSelector.asJson).
        add("shapeLabel", a.shapeLabel.asJson).
        add("status", a.info.status.asJson).
        add("reason", a.info.reason.fold(Json.Null)(Json.fromString(_))).
        add("appInfo", a.info.appInfo))
    }
  }

}
