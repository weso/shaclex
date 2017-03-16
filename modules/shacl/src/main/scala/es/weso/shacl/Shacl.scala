package es.weso.shacl
import es.weso.rdf.nodes._
import es.weso.rdf.PrefixMap

import util._
import SHACLPrefixes._
import es.weso.rdf.path.SHACLPath
import es.weso.shacl.converter.Shacl2RDF
import sext._

import scala.util.{Success, Try}

object Shacl {
  case object Unbounded
  lazy val defaultMin = 0
  lazy val defaultMax = Unbounded
  lazy val defaultFormat = "TURTLE"
}

case class Schema(
  pm: PrefixMap,
  shapes: Seq[Shape]) {


  /**
   * Get the shape associated to an IRI
   * @param iri IRI that identifies a shape
   */
  def shape(iri: IRI, allowDuplicates: Boolean = false): Either[String, Shape] = {
    def shapeIds : Seq[String] = {
      shapes.map(_.id).map(_.map(_.str)).map(_.getOrElse(""))
    }

    val found = shapes.filter(_.id contains(iri))
    found.size match {
      case 0 => Left(s"Not found $iri in Schema. Available shape Ids: ${shapeIds.mkString(",")}")
      case 1 => Right(found.head)
      case _ => if (allowDuplicates) {
        Right(found.head)
      } else {
        Left(s"More than one shape found with IRI $iri. Found = $found")
      }
    }
  }

  def siblingQualifiedShapes(s: PropertyShape): Either[String,Seq[Shape]] = parent(s) match {
    case Left(msg) => Left(msg)
    case Right(shape) => Right(shape.propertyShapes.filter(p => p != s && hasQualifiedShape(p)))
  }

  def hasQualifiedShape(p: PropertyShape): Boolean = p.components.find(c => c match {
    case x: QualifiedValueShape => true
    case _ => false
  }).isDefined

  /* Find shape x such that x sh:property p
   */
  def parent(p: PropertyShape): Either[String,Shape] = {
    findShapeWithPropertyShape(this.shapes,p)
  }

  private def findShapeWithPropertyShape(ls: Seq[Shape], p: PropertyShape): Either[String,Shape] = {
    val zero: Either[String,Shape] = Left(s"Not found shape with propertyShape $p in shapes: ${ls.toString}")
    def comb(s: Shape, rest: Either[String,Shape]): Either[String,Shape] = hasPropertyShape(s,p) match {
      case Left(_) => rest
      case Right(shape) => Right(shape)
    }
    ls.foldRight(zero)(comb)
  }

  private def hasPropertyShape(s: Shape, p: PropertyShape): Either[String,Shape] = {
    if (s.propertyShapes.contains(p)) Right(s)
    else
      findShapeWithPropertyShape(s.propertyShapes, p)
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

  def serialize(format: String = "TURTLE"): Try[String] = {
    format.toUpperCase match {
      case "TREE" => {
        Success(s"PrefixMap ${pm.treeString}\nShapes: ${shapes.treeString}")
      }
      case _ =>
        new Shacl2RDF {}.serialize(this, format)
    }
  }
}

// TODO: Refactor to define Shape as
//     sealed abstract class
// NodeShape extends Shape
// PropertyShape extends Shape
sealed abstract class Shape {
  def id: Option[IRI]
  def targets: Seq[Target]
  def components: Seq[Component]
  def propertyShapes: Seq[PropertyShape]
  def closed: Boolean
  def ignoredProperties: List[IRI]

  def addId(iri: IRI): Shape

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

  def predicatesInPropertyConstraints: List[IRI] =
      propertyShapes.map(_.predicate).toList

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

case class NodeShape(id: Option[IRI],
                     components: List[Component],
                     targets: Seq[Target],
                     propertyShapes: Seq[PropertyShape],
                     closed: Boolean,
                     ignoredProperties: List[IRI]
                    ) extends Shape {

  override def addId(iri: IRI): Shape = {
    this.copy(id = Some(iri))
  }

  def isPropertyConstraint = false
}

case class PropertyShape(id:Option[IRI],
                         path: SHACLPath,
                         components: Seq[Component],
                         targets: Seq[Target],
                         propertyShapes: Seq[PropertyShape],
                         closed: Boolean,
                         ignoredProperties: List[IRI]
) extends Shape {

  override def addId(iri: IRI): Shape = {
    this.copy(id = Some(iri))
  }

  def isPropertyConstraint = true

  def predicate : IRI = path.predicate.get

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
case class UniqueLang(value: Boolean) extends Component
case class LanguageIn(langs: List[String]) extends Component
case class Equals(p: IRI) extends Component
case class Disjoint(p: IRI) extends Component
case class LessThan(p: IRI) extends Component
case class LessThanOrEquals(p: IRI) extends Component
case class Or(shapes: List[Shape]) extends Component
case class And(shapes: List[Shape]) extends Component
case class Not(shape: Shape) extends Component
case class Xone(shapes: List[Shape]) extends Component
case class Closed(isClosed: Boolean, ignoredProperties: List[IRI]) extends Component
case class NodeComponent(shape: Shape) extends Component
case class HasValue(value: Value) extends Component
case class In(list: List[Value]) extends Component

// TODO: Change representation to include optional parent shape
case class QualifiedValueShape(shape: Shape,
                               qualifiedMinCount: Option[Int],
                               qualifiedMaxCount: Option[Int],
                               qualifiedValueShapesDisjoint: Option[Boolean]
                              ) extends Component


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
      shapes=Seq()
    )
}

object Shape {

  val empty = NodeShape(id = None,
    components = List(),
    targets = Seq(),
    propertyShapes = Seq(),
    closed = false,
    ignoredProperties = List()
  )

  def emptyPropertyShape(path: SHACLPath): PropertyShape = PropertyShape(
    id = None,
    path = path,
    components = Seq(),
    targets = Seq(),
    propertyShapes = Seq(),
    closed = false,
    ignoredProperties = List()
  )
}

