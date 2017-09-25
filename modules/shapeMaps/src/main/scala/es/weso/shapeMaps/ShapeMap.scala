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
import es.weso.rdf.PREFIXES._

abstract class ShapeMap {
  val associations: List[Association]
  val nodesPrefixMap: PrefixMap
  val shapesPrefixMap: PrefixMap

  def addAssociation(a: Association): Either[String, ShapeMap]

  def toJson: Json = {
    this.asJson
  }

  override def toString = Show[ShapeMap].show(this)

}

object ShapeMap {

  def empty: ShapeMap = FixedShapeMap.empty

  def parse(
    str: String,
    nodesPrefixMap: PrefixMap = PrefixMap.empty,
    shapesPrefixMap: PrefixMap = PrefixMap.empty): Either[String, QueryShapeMap] = {
    Parser.parse(str, nodesPrefixMap, shapesPrefixMap)
  }

  def parseResultMap(
    str: String,
    rdf: RDFReader,
    shapesPrefixMap: PrefixMap = PrefixMap.empty): Either[String, ResultShapeMap] = for {
    queryMap <- Parser.parse(str, rdf.getPrefixMap, shapesPrefixMap)
    fixMap <- fixShapeMap(queryMap, rdf, rdf.getPrefixMap, shapesPrefixMap)
  } yield ResultShapeMap(fixMap.shapeMap, rdf.getPrefixMap, shapesPrefixMap)

  /**
   * Resolve triple patterns according to an RDF
   */
  def fixShapeMap(
    shapeMap: ShapeMap,
    rdf: RDFReader,
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap): Either[String, FixedShapeMap] = {

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

  implicit val showShapeMap: Show[ShapeMap] = new Show[ShapeMap] {

    final def show(a: ShapeMap): String = {

      implicit val showPattern: Show[Pattern] = new Show[Pattern] {
        final def show(n: Pattern): String = {
          n match {
            case NodePattern(node) => a.nodesPrefixMap.qualify(node)
            case WildCard => "_"
            case Focus => "FOCUS"
          }
        }
      }

      implicit val showPredicate: Show[IRI] = new Show[IRI] {
        final def show(iri: IRI): String = iri match {
          case `rdf_type` => "a"
          case _ => a.nodesPrefixMap.qualify(iri)
        }
      }

      implicit val showNodeSelector: Show[NodeSelector] = new Show[NodeSelector] {
        final def show(n: NodeSelector): String = {
          n match {
            case RDFNodeSelector(node) => a.nodesPrefixMap.qualify(node)
            case TriplePattern(sub, pred, obj) => s"{${sub.show} ${pred.show} ${obj.show}}"
          }
        }
      }

      implicit val showShapeMapLabel: Show[ShapeMapLabel] = new Show[ShapeMapLabel] {
        final def show(label: ShapeMapLabel): String = label match {
          case IRILabel(iri) => a.shapesPrefixMap.qualify(iri)
          case BNodeLabel(bn) => bn.getLexicalForm
          case Start => "Start"
        }
      }

      implicit val showAssociation: Show[Association] = new Show[Association] {
        final def show(a: Association): String = {
          s"${a.nodeSelector.show} @ ${a.shapeLabel.show}"
        }
      }

      a.associations.map(_.show).mkString(",")

    }
  }
}
