package es.weso.schema
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import com.typesafe.scalalogging.LazyLogging
import es.weso.shapeMaps._
import es.weso.utils.EitherUtils
import io.circe._
import io.circe.JsonObject._

import util._

sealed abstract class ValidationTrigger {
  def explain: String
  def name: String
  def toJson: Json
  def shapeMap: ShapeMap
}

/**
 * Validates only target declarations
 */
case object TargetDeclarations extends ValidationTrigger {
  override def explain = "Only SHACL target declarations"
  override def name = "TargetDecls"
  override def toJson = Json.fromJsonObject(
    singleton("type", Json.fromString("TargetDecls")))
  override def shapeMap = ShapeMap.empty
}

case class ShapeMapTrigger(shapeMap: ShapeMap) extends ValidationTrigger {
  override def explain = "Shape Map"
  override def name = "ShapeMap"
  override def toJson = shapeMap.toJson
}

object ShapeMapTrigger {
  def apply: ShapeMapTrigger = empty
  def empty = ShapeMapTrigger(ShapeMap.empty)
}

object ValidationTrigger extends LazyLogging {

  lazy val default: ValidationTrigger = TargetDeclarations

  def nodeShape(node: RDFNode, shape: String, nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): ValidationTrigger =
    ShapeMapTrigger(QueryShapeMap(
      List(Association(RDFNodeSelector(node), IRILabel(IRI(shape)))),
      nodesPrefixMap,
      shapesPrefixMap))

  lazy val targetDeclarations: ValidationTrigger = TargetDeclarations

  def cnvShapes(ss: List[String], pm: PrefixMap): Either[String, Set[String]] = {
    EitherUtils.sequence(ss.map(removeLTGT(_, pm).map(_.str))).map(_.toSet)
  }

  def findTrigger(
    name: String,
    shapeMapStr: String,
    optBase: Option[String],
    optNode: Option[String],
    optShape: Option[String],
    nodePrefixMap: PrefixMap,
    shapePrefixMap: PrefixMap): Either[String, ValidationTrigger] = {
    // logger.info(s"Finding trigger $name, shapeMapStr: $shapeMapStr")
    name.toUpperCase match {
      case "TARGETDECLS" => Right(TargetDeclarations)
      case "SHAPEMAP" =>
        for {
          base <- optBase match {
            case None => Right(None)
            case Some(baseStr) => IRI.fromString(baseStr).map(Some(_))
          }
          shapeMap <- ShapeMap.fromCompact(shapeMapStr, base, nodePrefixMap, shapePrefixMap)
        } yield ShapeMapTrigger(shapeMap)
      case "NODESHAPE" => (optNode, optShape) match {
        case (Some(strNode), Some(strShape)) => for {
          node <- removeLTGT(strNode, nodePrefixMap)
          shape <- removeLTGT(strShape, shapePrefixMap)
        } yield {
          val shapeMapTrigger = nodeShape(node, shape.str, nodePrefixMap, shapePrefixMap)
          logger.debug(s"NodeShape triggerMode converted to $shapeMapTrigger")
          shapeMapTrigger
        }
        case _ => Left(s"Cannot be Shape trigger if no node or shape. Node = $optNode, shape = $optShape")
      }
      case "NODESTART" => (optNode, optShape) match {
        case (Some(strNode), None) => for {
          node <- removeLTGT(strNode, nodePrefixMap)
        } yield ShapeMapTrigger(QueryShapeMap(List(Association(RDFNodeSelector(node), Start)), nodePrefixMap, shapePrefixMap))
        case _ => Left(s"Cannot be NodeStart trigger with nnode = $optNode, shape = $optShape")
      }
      case _ =>
        Left(s"Cannot understand trigger mode\ntrigger = $name")
    }
  }

  /*
  * Remove < and > from string...if it is: "<http://example.org> -> http://example.org"
  */
  def removeLTGT(str: String, pm: PrefixMap): Either[String, IRI] = {
    val iriPattern = "<(.*)>".r
    str match {
      case iriPattern(c) => Right(IRI(c))
      case _ => pm.qname(str) match {
        case None =>
          Left(s"Can't obtain IRI from $str. Available prefixes: ${pm.prefixes.mkString(",")}")
        case Some(iri) =>
          Right(iri)
      }
    }
  }

  def triggerValues: List[(String, String)] = {
    List(TargetDeclarations, ShapeMapTrigger.empty).map(
      vt => (vt.name, vt.explain))
  }

  implicit val encodeSolution: Encoder[ValidationTrigger] = new Encoder[ValidationTrigger] {
    final def apply(a: ValidationTrigger): Json =
      a.toJson
  }

}
