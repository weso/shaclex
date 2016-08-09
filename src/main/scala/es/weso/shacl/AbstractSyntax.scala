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
   * Get the sequence of sh:targetNode declarations
   */
  def targetNodeShapes: Seq[(RDFNode,Shape)] = {
    val zero : Seq[(RDFNode,Shape)] = Seq()
    def comb(rs:Seq[(RDFNode,Shape)], s: Shape): Seq[(RDFNode,Shape)] = {
      val ns : Seq[RDFNode] = s.targetNodes
      ns.map(n => (n,s)) ++ rs
    }
    shapes.foldLeft(zero)(comb)
  }

 /**
   * Get the sequence of `sh:targetNode` declarations
   * @return a list of pairs (n,s) where n is the IRI of a node 
   * and s is the IRI of a shape
   */
  def targetNodeDeclarations: Seq[(RDFNode,IRI)] = {
    targetNodeShapes.map(p => (p._1,p._2.id.get))
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
    targets: Seq[Target],
    filters: Seq[Shape],
    components: Seq[Constraint] 
) {
  def hasId(iri: IRI): Boolean = {
    id contains(iri)
  }
  
  def targetNodes: Seq[RDFNode] = { 
    val maybeScopeNodes = targets.map(_.toTargetNode)
    maybeScopeNodes.flatten.map(_.node)
  }
  
  def propertyConstraints: Seq[PropertyConstraint] = {
    components.map(_.toPropertyConstraint).flatten
  }
  
}

sealed abstract class Target {
  def isTargetNode: Boolean
  def toTargetNode: Option[TargetNode]
}


case class TargetNode(node: RDFNode) extends Target {
  def isTargetNode = true
  def toTargetNode = Some(this)
  
}

// TODO...add more types of targets
// case class TargetClass(cls: IRI) extends Target
// case class PropertyTarget(predicate: IRI) extends Target

sealed abstract class Constraint {
  def isPropertyConstraint: Boolean
  def toPropertyConstraint: Option[PropertyConstraint] = None
  def components: Seq[Component]
}

case class PropertyConstraint(
    id:Option[IRI],
    predicate: IRI,
    components: Seq[Component]
) extends Constraint {
  def isPropertyConstraint = true
  
  override def toPropertyConstraint: Option[PropertyConstraint] = Some(this)
}

case class PathPropertyConstraint(
    id:Option[IRI],
    path: Path,
    components: Seq[Component]
) extends Constraint {
  def isPropertyConstraint = false
  
}

case class NodeConstraint(
    components: Seq[Component]
) extends Constraint {
  def isPropertyConstraint = false
}

case class Filter(shape: Shape)

sealed abstract class Component

/**
 * Represents IRIs or Literals (no Blank nodes)
 */
trait Value  {
  def matchNode(node: RDFNode): Boolean
} 

case class IRIValue(iri: IRI) extends Value {
  override def matchNode(node: RDFNode) = {
    node match {
      case i: IRI => iri == i
      case _ => false
    }
  }
}

case class LiteralValue(literal: Literal) extends Value {
  override def matchNode(node: RDFNode) = {
    node match {
      case l: Literal => l == literal
      case _ => false
    }
  }
}

case class ClassComponent(value: RDFNode) extends Component  
case class Datatype(value: IRI) extends Component 
case class NodeKind(value: NodeKindType) extends Component 
case class MinCount(value: Int) extends Component 
case class MaxCount(value: Int) extends Component 
case class MinExclusive(value: Literal) extends Component 
case class MinInclusive(value: Literal) extends Component 
case class MaxExclusive(value: Literal) extends Component 
case class MaxInclusive(value: Literal) extends Component 
case class MinLength(value: Int) extends Component 
case class MaxLength(value: Int) extends Component 
case class Pattern(pattern: String, flags: Option[String]) extends Component  
case class UniqueLang(value: Boolean) extends Component 
case class Closed(isClosed: Boolean, ignoredProperties: Seq[IRI]) extends Component
case class ShapeComponent(shape: Shape) extends Component
case class HasValue(value: Value) extends Component 
case class In(list: List[Value]) extends Component  

sealed trait NodeKindType extends Component 
case object IRIKind extends NodeKindType
case object LiteralKind extends NodeKindType
case object BlankNodeKind extends NodeKindType
case object BlankNodeOrIRI extends NodeKindType
case object BlankNodeOrLiteral extends NodeKindType
case object IRIOrLiteral extends NodeKindType

// Companion objects
object Schema {
  val empty = Schema(shapes=Seq())
}

object Shape {
  val empty = Shape(None,Seq(),Seq(),Seq())
}

sealed trait Path
case class PredicatePath(iri: IRI) extends Path
case class InversePath(iri: IRI) extends Path
case class SequencePath(paths: Seq[Path]) extends Path
case class AlternativePath(paths: Seq[Path]) extends Path
case class ZeroOrMorePath(iri: Path) extends Path
case class OneOrMorePath(iri: Path) extends Path