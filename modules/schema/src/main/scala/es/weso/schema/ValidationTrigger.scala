package es.weso.schema
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import cats._
import cats.data._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.syntax._
import io.circe.JsonObject._
import util._

abstract class ValidationTrigger {
  def explain: String
  def name: String
  def toJson: Json
}

/**
 * Validates only scope declarations
 */
case object TargetDeclarations extends ValidationTrigger {
  override def explain = "Only SHACL target declarations"
  override def name = "TargetDecls"
  override def toJson = Json.fromJsonObject(
    singleton("type",Json.fromString("TargetDecls"))
  )
}

case class ShapeMapTrigger(map: Map[RDFNode,Set[String]],
                           nodes: Set[RDFNode]) extends ValidationTrigger {
  override def explain = "A shape map"
  override def name = "ShapeMap"

  implicit val rdfNodeKeyEncoder = new KeyEncoder[RDFNode] {
    override def apply(node: RDFNode): String = node.toString
  }

  // TODO: Improve this...
  implicit val rdfNodeEncoder = new Encoder[RDFNode] {
    override def apply(node: RDFNode): Json =
      Json.fromJsonObject(
       singleton("type", Json.fromString("RDFNode")).
       add("value", Json.fromString(node.toString))
      )
  }

  override def toJson = Json.fromJsonObject(
    singleton("type",Json.fromString("ShapeMap")).
      add("shapeMap",map.asJson).
      add("nodesStart",nodes.asJson)
  )

}

object ShapeMapTrigger {
  def empty = ShapeMapTrigger(Map(),Set())
}

object ValidationTrigger extends LazyLogging {

 lazy val default: ValidationTrigger = TargetDeclarations

 def nodeShape(node: String, shape: String): ValidationTrigger =
   ShapeMapTrigger(Map(IRI(node) -> Set(shape)), Set())

 lazy val targetDeclarations: ValidationTrigger = TargetDeclarations

 def cnvShapes(ss: List[String], pm: PrefixMap): Either[String,Set[String]] = {
   ss.map(removeLTGT(_,pm).map(_.str)).sequenceU.map(_.toSet)
 }

 def findTrigger(name: String,
                 shapeMap: Map[String,List[String]],
                 optNode: Option[String],
                 optShape: Option[String],
                 nodePrefixMap: PrefixMap = PrefixMap.empty,
                 shapePrefixMap: PrefixMap = PrefixMap.empty
                ): Either[String,ValidationTrigger] = {
   name.toUpperCase match {
     case "TARGETDECLS" => Right(TargetDeclarations)

     case "SHAPEMAP" => {
       val cleanedShapeMap = shapeMap - ""
       val cnvShapeMap: List[Either[String,(RDFNode,Set[String])]] = cleanedShapeMap.map{ case (node, ls) => for {
          rdfNode <- removeLTGT(node,nodePrefixMap)
          shapes <- cnvShapes(ls,shapePrefixMap)
        } yield (rdfNode,shapes)
       }.toList
       val eitherList: Either[String,List[(RDFNode,Set[String])]] = cnvShapeMap.sequenceU
       for {
         ls <- eitherList
       } yield ShapeMapTrigger(Map(ls: _*),Set())
     }
     case "NODESHAPE" => (optNode,optShape) match {
       case (Some(strNode), Some(strShape)) => for {
         node <- removeLTGT(strNode,nodePrefixMap)
         shape <- removeLTGT(strShape,shapePrefixMap)
       } yield {
         val shapeMap = ShapeMapTrigger(Map(node -> Set(shape.str)), Set())
         logger.info(s"Shape trigger converted to $shapeMap")
         shapeMap
       }
       case _ => Left(s"Cannot be Shape trigger if no node or shape. Node = $optNode, shape = $optShape")
     }
     case "NODESTART" => (optNode,optShape) match {
       case (Some(strNode), None) => for {
         node <- removeLTGT(strNode,nodePrefixMap)
       } yield ShapeMapTrigger(Map(), Set(node))
       case _ => Left(s"Cannot be NodeStart trigger with nnode = $optNode, shape = $optShape")
     }
     case _ =>
       Left(s"Cannot understand trigger mode\ntrigger = $name")
   }
 }

 /*
  * Remove < and > from string...if it is: "<http://example.org> -> http://example.org"
  */
 def removeLTGT(str: String, pm: PrefixMap): Either[String,IRI] = {
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

 def triggerValues: List[(String,String)] = {
   List(TargetDeclarations,ShapeMapTrigger.empty).map(
      vt => (vt.name,vt.explain)
     )
 }

 implicit val encodeSolution: Encoder[ValidationTrigger] = new Encoder[ValidationTrigger] {
    final def apply(a: ValidationTrigger): Json =
      a.toJson
 }

}
