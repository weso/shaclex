package es.weso.schema
import es.weso.rdf.nodes._
import util._

abstract class ValidationTrigger {

  def explain: String
  def extractNode: String =
    maybeFocusNode.getOrElse("")

  def extractShape: String =
    maybeShape.getOrElse("")

  def maybeFocusNode: Option[String]
  def maybeShape: Option[String]

  def name: String
}

/**
 * Validates only scope declarations
 */
case object ScopeDeclarations extends ValidationTrigger {
  override def explain = "Only scope node declarations"

  override def maybeFocusNode = None
  override def maybeShape = None

  override def name = "ScopeDeclarations"

}

/**
 * Validates all nodes against all shapes
 *
 */
case object AllNodesAllShapes extends ValidationTrigger {
  override def explain = "All nodes in data against all shapes"
  override def maybeFocusNode = None
  override def maybeShape = None
  override def name = "AllNodesAllShapes"
}

/**
 * Validates a node against all shapes
 *
 */
case class NodeAllShapes(node: RDFNode) extends ValidationTrigger {
  override def explain = "A node with all shapes"
  override def maybeFocusNode = Some(node.toString)
  override def maybeShape = None
  override def name = "NodeAllShapes"
}

/**
 * Validates a node against a specific shape
 */
case class NodeShape(node: RDFNode, shape: String) extends ValidationTrigger {
  override def explain = "A node with a shape"
  override def maybeFocusNode = Some(node.toString)
  override def maybeShape = Some(shape)
  override def name = "NodeShape"
}

/**
 * Validates all nodes against a specific shape
 */
case class AllNodesShape(shape: String) extends ValidationTrigger {
  override def explain = "All nodes with a shape"
  override def maybeFocusNode = None
  override def maybeShape = Some(shape)
  override def name = "AllNodesShape"
}

object ValidationTrigger {

 lazy val default: ValidationTrigger = ScopeDeclarations

 // Validation Trigger constructors (could replace by apply)
 def nodeAllShapes(node: String): ValidationTrigger =
   NodeAllShapes(IRI(node))

 // Validation Trigger constructors (could replace by apply)
 def allNodesShape(shape: String): ValidationTrigger =
   AllNodesShape(shape)


 def nodeShape(node: String, shape: String): ValidationTrigger =
   NodeShape(IRI(node), shape)

 lazy val scopeDeclarations: ValidationTrigger = ScopeDeclarations

 lazy val allNodesAllShapes: ValidationTrigger = AllNodesAllShapes

 def fromOptIRI(optIRI: Option[String]): ValidationTrigger = {
   optIRI match {
     case None => default
     case Some(iri) => nodeAllShapes(iri)
   }
 }

 def findTrigger(name: String, node: Option[String], shape: Option[String]): Try[ValidationTrigger] = {
   (name,node,shape) match {
     case ("ScopeDeclarations",_,_) => Success(ScopeDeclarations)
     case ("AllNodesAllShapes",_,_) => Success(AllNodesAllShapes)
     case ("NodeShape",Some(node),Some(shape)) => {
       val iri = removeLTGT(node)
       Success(NodeShape(iri,shape))
     }
     case ("NodeAllShapes",Some(node),_) => {
       val iri = removeLTGT(node)
       Success(NodeAllShapes(iri))
     }
     case ("AllNodesShape",_,Some(shape)) => Success(AllNodesShape(shape))
     case _ => Failure(
       new RuntimeException(s"Cannot understand trigger mode. trigger = $name, node: $node, shape: $shape"))
   }
 }

 /**
  * Remove < and > from string...if it is: "<http://example.org> -> http://example.org"
  */
 def removeLTGT(str: String): RDFNode = {
   val pattern = "<(.*)>".r
   str match {
     case pattern(c) => IRI(c)
   }
 }

 def triggerValues: List[(String,String)] = {
   val allNodesShape = AllNodesShape("")
   val nodeAllShapes = NodeAllShapes(IRI(""))
   val nodeShape = NodeShape(IRI(""),"")
   val ls = List(ScopeDeclarations, AllNodesAllShapes, allNodesShape, nodeAllShapes,nodeShape)
   ls.map(vt => (vt.name,vt.explain))
 }

}
