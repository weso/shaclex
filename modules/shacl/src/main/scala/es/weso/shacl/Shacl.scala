package es.weso.shacl

import es.weso.rdf.nodes._

import util._
import SHACLPrefixes._
import es.weso.rdf.PrefixMap
import es.weso.rdf.path.SHACLPath
import es.weso.shacl.converter.Shacl2RDF
import sext._


object Shacl {
  case object Unbounded
  lazy val defaultMin = 0
  lazy val defaultMax = Unbounded
  lazy val defaultFormat = "TURTLE"
}

case class Schema(
  pm: PrefixMap,
  shapesMap: Map[ShapeRef, Shape]) {

  lazy val shapes: Seq[Shape] =
    shapesMap.toSeq.map(_._2)

  lazy val shapeRefs: Seq[ShapeRef] =
    shapesMap.keys.toSeq

  /**
   * Get the shape associated to an IRI
   * @param iri IRI that identifies a shape
   */
  def shape(node: RDFNode): Either[String, Shape] =
    shapesMap.get(ShapeRef(node)) match {
      case None => Left(s"Not found $node in Schema")
      case Some(shape) => Right(shape)
    }

  def siblingQualifiedShapes(s: ShapeRef): List[ShapeRef] = {
    val parentShapes: List[Shape] =
      parents(s).
        map(shapesMap.get(_)).
        collect { case Some(shape) => shape }
    val qualifiedPropertyShapes =
      parentShapes.
        flatMap(_.propertyShapes).
        filter(_ != s)
    collectQualifiedValueShapes(qualifiedPropertyShapes)
  }

  def collectQualifiedValueShapes(ls: List[ShapeRef]): List[ShapeRef] = {
    val zero: List[ShapeRef] = List()
    def comb(xs: List[ShapeRef], x: ShapeRef): List[ShapeRef] =
      qualifiedShapes(x) ++ xs
    ls.foldLeft(zero)(comb)
  }

  def qualifiedShapes(p: ShapeRef): List[ShapeRef] = shapesMap.get(p) match {
    case None => List()
    case Some(shape) =>
      shape.components.collect { case x: QualifiedValueShape => x.shape }.toList
  }

  /* Find shape x such that x sh:property p
   */
  def parents(p: ShapeRef): List[ShapeRef] = {
    shapesWithPropertyShape(this.shapeRefs, p)
  }

  private def shapesWithPropertyShape(ls: Seq[ShapeRef], p: ShapeRef): List[ShapeRef] = {
    ls.filter(hasPropertyShape(_, p)).toList
  }

  private def hasPropertyShape(s: ShapeRef, p: ShapeRef): Boolean = {
    shapesMap.get(s) match {
      case None => false // TODO: Maybe raise an error
      case Some(shape) =>
        if (shape.propertyShapes.contains(p)) true
        else false
    }
  }

  /**
   * Get the sequence of sh:targetNode declarations
   */
  def targetNodeShapes: Seq[(RDFNode, Shape)] = {
    val zero: Seq[(RDFNode, Shape)] = Seq()
    def comb(rs: Seq[(RDFNode, Shape)], s: Shape): Seq[(RDFNode, Shape)] = {
      val ns: Seq[RDFNode] = s.targetNodes
      ns.map(n => (n, s)) ++ rs
    }
    shapes.foldLeft(zero)(comb)
  }

  /**
   * Get the sequence of `sh:targetNode` declarations
   * @return a list of pairs (n,s) where n is the IRI of a node
   * and s is the IRI of a shape
   */
  def targetNodeDeclarations: Seq[(RDFNode, IRI)] = {
    targetNodeShapes.collect { case (node, shape) if shape.id.isIRI => (node, shape.id.toIRI) }
  }

  def serialize(format: String = "TURTLE"): Either[String, String] = {
    format.toUpperCase match {
      case "TREE" => {
        Right(s"PrefixMap ${pm.treeString}\nShapes: ${shapes.treeString}")
      }
      case _ =>
        new Shacl2RDF {}.serialize(this, format)
    }
  }
}

sealed abstract class Shape {
  def id: RDFNode
  def targets: Seq[Target]
  def components: Seq[Component]
  def propertyShapes: Seq[ShapeRef]
  def closed: Boolean
  def ignoredProperties: List[IRI]

  def hasId(iri: IRI): Boolean = {
    id == iri
  }

  def showId: String =
    id match {
      case iri: IRI => iri.str
      case bnode: BNodeId => bnode.toString
    }

  def targetNodes: Seq[RDFNode] =
    targets.map(_.toTargetNode).flatten.map(_.node)

  def targetClasses: Seq[RDFNode] =
    targets.map(_.toTargetClass).flatten.map(_.node)

  def targetSubjectsOf: Seq[IRI] =
    targets.map(_.toTargetSubjectsOf).flatten.map(_.pred)

  def targetObjectsOf: Seq[IRI] =
    targets.map(_.toTargetObjectsOf).flatten.map(_.pred)

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

/**
 * Captures the common parts of NodeShapes and PropertyShapes
 */
/*sealed abstract class Constraint {
  def isPropertyConstraint: Boolean
  def toPropertyConstraint: Option[PropertyShape] = None
  def components: Seq[Component]
} */

case class NodeShape(
  id: RDFNode,
  components: List[Component],
  targets: Seq[Target],
  propertyShapes: Seq[ShapeRef],
  closed: Boolean,
  ignoredProperties: List[IRI]) extends Shape {

  def isPropertyConstraint = false
}

case class PropertyShape(
  id: RDFNode,
  path: SHACLPath,
  components: Seq[Component],
  targets: Seq[Target],
  propertyShapes: Seq[ShapeRef],
  closed: Boolean,
  ignoredProperties: List[IRI]) extends Shape {

  def isPropertyConstraint = true

  def predicate: IRI = path.predicate.get

}

sealed abstract class Component

/**
 * Represents IRIs or Literals (no Blank nodes)
 */
trait Value {
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

case class ShapeRef(id: RDFNode) extends AnyVal {
  def showId = id.toString
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
case class LanguageIn(langs: List[String]) extends Component
case class Equals(p: IRI) extends Component
case class Disjoint(p: IRI) extends Component
case class LessThan(p: IRI) extends Component
case class LessThanOrEquals(p: IRI) extends Component
case class Or(shapes: List[ShapeRef]) extends Component
case class And(shapes: List[ShapeRef]) extends Component
case class Not(shape: ShapeRef) extends Component
case class Xone(shapes: List[ShapeRef]) extends Component
case class Closed(isClosed: Boolean, ignoredProperties: List[IRI]) extends Component
case class NodeComponent(shape: ShapeRef) extends Component
case class HasValue(value: Value) extends Component
case class In(list: List[Value]) extends Component

// TODO: Change representation to include optional parent shape
case class QualifiedValueShape(
  shape: ShapeRef,
  qualifiedMinCount: Option[Int],
  qualifiedMaxCount: Option[Int],
  qualifiedValueShapesDisjoint: Option[Boolean]) extends Component

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

// Companion iriObjects
object Schema {
  val empty =
    Schema(
      pm = SHACLPrefixes.defaultPrefixMap,
      shapesMap = Map[ShapeRef, Shape]())
}

object Shape {

  def empty(id: RDFNode) = NodeShape(
    id = id,
    components = List(),
    targets = Seq(),
    propertyShapes = Seq(),
    closed = false,
    ignoredProperties = List())

  def emptyPropertyShape(
    id: RDFNode,
    path: SHACLPath): PropertyShape = PropertyShape(
    id = id,
    path = path,
    components = Seq(),
    targets = Seq(),
    propertyShapes = Seq(),
    closed = false,
    ignoredProperties = List())
}

