package es.weso.shacl

import es.weso.rdf.nodes._
import util._

case class Schema(shapes: Seq[Shape]) {
  
  /**
   * Get the shape associated to an IRI
   * @param iri IRI that identifies a shape
   */
  def shape(iri: IRI): Option[Shape] = { 
    shapes.filter(_.id contains(iri)).headOption
  }
  
  /**
   * Get the sequence of sh:scopeNode declarations
   */
  def scopeNodeShapes: Seq[(IRI,Shape)] = {
    val zero : Seq[(IRI,Shape)] = Seq()
    def comb(rs:Seq[(IRI,Shape)], s: Shape): Seq[(IRI,Shape)] = {
      val ns : Seq[IRI] = s.scopeNodes
      ns.map(n => (n,s)) ++ rs
    }
    shapes.foldLeft(zero)(comb)
  }

 /**
   * Get the sequence of sh:scopeNode declarations
   * @return a list of pairs (n,s) where n is the IRI of a node 
   * and s is the IRI of a shape
   */
  def scopeNodeDeclarations: Seq[(IRI,IRI)] = {
    scopeNodeShapes.map(p => (p._1,p._2.id.get))
  }

  def serialize(format: String = "AST"): Try[String] = {
    format match {
      case "AST" => {
        Success(toString)
      }
      case _ => 
        Shacl2RDF.serialize(this, format)
    }
  }
}

case class Shape(
    id: Option[IRI],
    scopes: Seq[Scope],
    filters: Seq[Shape],
    components: Seq[Constraint] 
) {
  def hasId(iri: IRI): Boolean = {
    id contains(iri)
  }
  
  def scopeNodes: Seq[IRI] = { 
    val maybeScopeNodes = scopes.map(_.toScopeNode)
    maybeScopeNodes.flatten.map(_.node)
  }
  
  def propertyConstraints: Seq[PropertyConstraint] = {
    components.map(_.toPropertyConstraint).flatten
  }
  
}

sealed abstract class Scope {
  def isScopeNode: Boolean
  
  def toScopeNode: Option[ScopeNode]
}


case class ScopeNode(node: IRI) extends Scope {
  def isScopeNode = true
  
  def toScopeNode = Some(this)
  
}
// TODO...add more types of scopes
// case class ScopeClass(cls: IRI) extends Scope
// case class PropertyScope(predicate: IRI) extends Scope

sealed abstract class Constraint {
  def isPropertyConstraint: Boolean
  
  def toPropertyConstraint: Option[PropertyConstraint] = None
  
  def components: Seq[Component]
}

case class PropertyConstraint(
    id:Option[IRI],
    predicate: IRI,
    components: Seq[PCComponent]
) extends Constraint {
  def isPropertyConstraint = true
  
  override def toPropertyConstraint: Option[PropertyConstraint] = Some(this)
}

case class InversePropertyConstraint(
    id:Option[IRI],
    predicate: IRI,
    components: Seq[IPCComponent]
) extends Constraint {
  def isPropertyConstraint = false
  
}

case class NodeConstraint(
    id:Option[IRI],
    predicate: IRI,
    components: Seq[NCComponent]
) extends Constraint {
  def isPropertyConstraint = false
}

case class Filter(shape: Shape)

sealed abstract class Component

/**
 *  PropertyConstraint Component 
 */
sealed trait PCComponent extends Component

/**
 * InversePropertyConstraint Component
 */
sealed trait IPCComponent extends Component

/**
 * NodeConstraint Component
 */
sealed trait NCComponent extends Component


 
trait Value  // Represents IRIs or Literals (not blank nodes)
case class IRIValue(iri: IRI) extends Value
case class LiteralValue(literal: Literal) extends Value

case class ShClass(value: RDFNode) 
 extends NCComponent with PCComponent with IPCComponent
 
case class ClassIn(values: Seq[RDFNode]) 
 extends NCComponent with PCComponent with IPCComponent

case class Datatype(value: RDFNode) 
 extends PCComponent with IPCComponent

case class DatatypeIn(values: Seq[RDFNode]) 
 extends PCComponent with IPCComponent
 
case class DirectType(value: RDFNode) 
 extends NCComponent with PCComponent with IPCComponent

case class NodeKind(value: NodeKindType) 
 extends NCComponent with PCComponent with IPCComponent

case class MinCount(value: Int)
 extends PCComponent with IPCComponent
 
case class MaxCount(value: Int)
 extends PCComponent with IPCComponent
 
case class MinExclusive(value: Int)
 extends PCComponent with IPCComponent
 
case class MinInclusive(value: Int)
 extends PCComponent with IPCComponent
 
case class MaxExclusive(value: Int)
 extends PCComponent with IPCComponent

case class MaxInclusive(value: Int)
 extends PCComponent with IPCComponent
 
case class MinLength(value: Int)
 extends PCComponent with IPCComponent
 
case class MaxLength(value: Int)
 extends PCComponent with IPCComponent

case class Pattern(pattern: String, flags: Option[String])
 extends NCComponent with PCComponent with IPCComponent
 
case class UniqueLang(value: Boolean)
 extends PCComponent 


 

/**
 * 
 */
case class Closed(
    isClosed: Boolean, 
    ignoredProperties: Seq[IRI]) 
  extends NCComponent

/**
 * sh:hasValue
 * 
 * TODO: The spec allows also blank nodes     
 */
case class HasValue(value: Value)
     extends PCComponent with IPCComponent

 
case class In(list: Seq[Value]) 
     extends PCComponent with IPCComponent with NCComponent
 

sealed trait NodeKindType
case object IRIKind extends NodeKindType
case object LiteralKind extends NodeKindType
case object BlankNodeKind extends NodeKindType
case object BlankNodeOrIRI extends NodeKindType
case object BlankNodeOrLiteral extends NodeKindType
case object IRIOrLiteral extends NodeKindType


object Shape {
  val empty = Shape(None,Seq(),Seq(),Seq())
}
