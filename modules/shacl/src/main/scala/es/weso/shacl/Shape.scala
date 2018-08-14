package es.weso.shacl

import es.weso.rdf.nodes.{BNode, IRI, Literal, RDFNode}
import es.weso.rdf.path.SHACLPath

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
      case bnode: BNode => bnode.toString
      case l: Literal => l.getLexicalForm
    }

  def targetNodes: Seq[RDFNode] =
    targets.map(_.toTargetNode).flatten.map(_.node)

  def targetClasses: Seq[RDFNode] =
    targets.map(_.toTargetClass).flatten.map(_.node)

  def targetSubjectsOf: Seq[IRI] =
    targets.map(_.toTargetSubjectsOf).flatten.map(_.pred)

  def targetObjectsOf: Seq[IRI] =
    targets.map(_.toTargetObjectsOf).flatten.map(_.pred)

  def componentShapes: Seq[ShapeRef] = {
    components.collect {
      case NodeComponent(sref) => sref
//      case Or(srefs) => srefs
//      case And(srefs) => srefs
//      case Not(sref) => List(sref) // TODO: Not sure if this should be included...
    }
  }

}

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
                          components: List[Component],
                          targets: Seq[Target],
                          propertyShapes: Seq[ShapeRef],
                          closed: Boolean,
                          ignoredProperties: List[IRI]) extends Shape {

  def isPropertyConstraint = true

  def predicate: Option[IRI] = path.predicate

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
    components = List(),
    targets = Seq(),
    propertyShapes = Seq(),
    closed = false,
    ignoredProperties = List())
}
