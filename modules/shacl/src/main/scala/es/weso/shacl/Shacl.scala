package es.weso.shacl
import es.weso.rdf.nodes._
import es.weso.rdf.PrefixMap

import util._
import SHACLPrefixes._
import es.weso.rdf.path.SHACLPath
import es.weso.shacl.converter.Shacl2RDF

import scala.util.{Success, Try}

object Shacl {
  case object Unbounded
  lazy val defaultMin = 0
  lazy val defaultMax = Unbounded
  lazy val defaultFormat = "TURTLE"
}

case class Schema(
  pm: PrefixMap,
  shapes: Seq[NodeShape]) {

  /**
   * Get the shape associated to an IRI
   * @param iri IRI that identifies a shape
   */
  def shape(iri: IRI): Option[NodeShape] = {
    shapes.filter(_.id contains(iri)).headOption
  }

  /**
   * Get the sequence of sh:targetNode declarations
   */
  def targetNodeShapes: Seq[(RDFNode,NodeShape)] = {
    val zero : Seq[(RDFNode,NodeShape)] = Seq()
    def comb(rs:Seq[(RDFNode,NodeShape)], s: NodeShape): Seq[(RDFNode,NodeShape)] = {
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
        new Shacl2RDF {}.serialize(this, format)
    }
  }
}

case class NodeShape(
                      id: Option[IRI],
                      targets: Seq[Target],
                      constraints: Seq[Shape],
                      closed: Boolean,
                      ignoredProperties: List[IRI]
) {

  def addId(iri: IRI): NodeShape = {
    this.copy(id = Some(iri))
  }

  def hasId(iri: IRI): Boolean = {
    id contains(iri)
  }

  def showId: String =
    id.fold("")(_.str)

  def targetNodes: Seq[RDFNode] =
    targets.map(_.toTargetNode).flatten.map(_.node)

  def targetClasses: Seq[RDFNode] =
    targets.map(_.toTargetClass).flatten.map(_.node)

  def targetSubjectsOf: Seq[IRI] =
    targets.map(_.toTargetSubjectsOf).flatten.map(_.pred)

  def targetObjectsOf: Seq[IRI] =
    targets.map(_.toTargetObjectsOf).flatten.map(_.pred)

  def propertyConstraints: Seq[PropertyShape] =
    constraints.map(_.toPropertyConstraint).flatten

  def predicatesInPropertyConstraints: List[IRI] =
      propertyConstraints.map(_.predicate).toList

}

sealed abstract class Target {
  def toTargetNode: Option[TargetNode] = this match {
    case tn: TargetNode => Some(tn)
    case _ => None
  }
  def toTargetClass: Option[TargetClass] = this match {
    case tc: TargetClass => Some(tc)
    case _ => None
  }
  def toTargetSubjectsOf: Option[TargetSubjectsOf] = this match {
    case t: TargetSubjectsOf => Some(t)
    case _ => None
  }
  def toTargetObjectsOf: Option[TargetObjectsOf] = this match {
    case t: TargetObjectsOf => Some(t)
    case _ => None
  }
}
case class TargetNode(node: RDFNode) extends Target
case class TargetClass(node: RDFNode) extends Target
case class TargetSubjectsOf(pred: IRI) extends Target
case class TargetObjectsOf(pred: IRI) extends Target

sealed abstract class Shape {
  def isPropertyConstraint: Boolean
  def toPropertyConstraint: Option[PropertyShape] = None
  def components: Seq[Component]
}

case class PropertyShape(
                          id:Option[IRI],
                          path: SHACLPath,
                          components: Seq[Component]
) extends Shape {
  def isPropertyConstraint = true

  def predicate : IRI = path.predicate.get

  override def toPropertyConstraint: Option[PropertyShape] = Some(this)
}

/*
case class PathPropertyShape(
    id:Option[IRI],
    path: Path,
    components: Seq[Component]
) extends Shape {
  def isPropertyConstraint = false
} */

case class NodeConstraint(
    components: List[Component]
) extends Shape {
  def isPropertyConstraint = false
}

sealed abstract class Component

/**
 * Represents IRIs or Literals (no Blank nodes)
 */
trait Value  {
  /**
   * `true` if `node` matches this value
   */
  def matchNode(node: RDFNode): Boolean

  /**
   * Conversion from values to RDFNode's
   */
  def rdfNode: RDFNode
}

case class IRIValue(iri: IRI) extends Value {
  override def matchNode(node: RDFNode) = {
    node match {
      case i: IRI => iri == i
      case _ => false
    }
  }

  override def rdfNode: RDFNode = iri
}

case class LiteralValue(literal: Literal) extends Value {
  override def matchNode(node: RDFNode) = {
    node match {
      case l: Literal => l == literal
      case _ => false
    }
  }

  override def rdfNode: RDFNode = literal
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
case class Stem(v: String) extends Component
case class UniqueLang(value: Boolean) extends Component
case class Equals(p: IRI) extends Component
case class Disjoint(p: IRI) extends Component
case class LessThan(p: IRI) extends Component
case class LessThanOrEquals(p: IRI) extends Component
case class Or(shapes: List[NodeShape]) extends Component
case class And(shapes: List[NodeShape]) extends Component
case class Not(shape: NodeShape) extends Component
case class Closed(isClosed: Boolean, ignoredProperties: List[IRI]) extends Component
case class ShapeComponent(shape: NodeShape) extends Component
case class HasValue(value: Value) extends Component
case class In(list: List[Value]) extends Component


sealed trait NodeKindType {
  def id: IRI
}
case object IRIKind extends NodeKindType {
  override def id = sh_IRI
}
case object LiteralKind extends NodeKindType {
  override def id = sh_Literal
}
case object BlankNodeKind extends NodeKindType {
  override def id = sh_BlankNode
}
case object BlankNodeOrIRI extends NodeKindType {
  override def id = sh_BlankNodeOrIRI
}
case object BlankNodeOrLiteral extends NodeKindType {
  override def id = sh_BlankNodeOrLiteral
}
case object IRIOrLiteral extends NodeKindType {
  override def id = sh_IRIOrLiteral
}

// Companion objects
object Schema {
  val empty =
    Schema(
      pm = SHACLPrefixes.defaultPrefixMap,
      shapes=Seq()
    )
}

object NodeShape {
  val empty = NodeShape(None,Seq(),Seq(),false,List())
}

