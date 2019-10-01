package es.weso.shacl

import es.weso.rdf.nodes._
import es.weso.rdf.path.SHACLPath
import es.weso.shacl.report.Severity

sealed abstract class Shape {
  def id: RDFNode
  def targets: Seq[Target]
  def components: Seq[Component]
  def propertyShapes: Seq[RefNode]
  def closed: Boolean
  def deactivated: Boolean
  def message: MessageMap
  def name: MessageMap
  def description: MessageMap
  def order: Option[DecimalLiteral]
  def group: Option[RefNode]
  def sourceIRI: Option[IRI]

  def severity: Option[Severity]
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

  def componentShapes: Seq[RefNode] = {
    components.collect {
      case NodeComponent(sref) => sref
//      case Or(srefs) => srefs
//      case And(srefs) => srefs
//      case Not(sref) => List(sref) // TODO: Not sure if this should be included...
    }
  }

  def addPropertyShapes(ps: Seq[RefNode]): Shape


}

case class NodeShape(
                      id: RDFNode,
                      components: List[Component],
                      targets: Seq[Target],
                      propertyShapes: Seq[RefNode],
                      closed: Boolean,
                      ignoredProperties: List[IRI],
                      deactivated: Boolean,
                      message: MessageMap,
                      severity: Option[Severity],
                      name: MessageMap,
                      description: MessageMap,
                      order: Option[DecimalLiteral],
                      group: Option[RefNode],
                      sourceIRI: Option[IRI]
                    ) extends Shape {

  def isPropertyConstraint = false

  override def addPropertyShapes(ps: Seq[RefNode]): Shape =
    this.copy(propertyShapes = this.propertyShapes ++ ps)

}

case class PropertyShape(
                          id: RDFNode,
                          path: SHACLPath,
                          components: List[Component],
                          targets: Seq[Target],
                          propertyShapes: Seq[RefNode],
                          closed: Boolean,
                          ignoredProperties: List[IRI],
                          deactivated: Boolean,
                          message: MessageMap,
                          severity: Option[Severity],
                          name: MessageMap,
                          description: MessageMap,
                          order: Option[DecimalLiteral],
                          group: Option[RefNode],
                          sourceIRI: Option[IRI],
                          annotations: List[(IRI,RDFNode)]
                        ) extends Shape {

  def isPropertyConstraint = true

  def predicate: Option[IRI] = path.predicate

  override def addPropertyShapes(ps: Seq[RefNode]): Shape = {
    this.copy(propertyShapes = this.propertyShapes ++ ps)
  }
}

object Shape {

  def empty(id: RDFNode) = NodeShape(
    id = id,
    components = List(),
    targets = Seq(),
    propertyShapes = Seq(),
    closed = false,
    ignoredProperties = List(),
    deactivated = false,
    message = MessageMap.empty,
    severity = None,
    name = MessageMap.empty,
    description = MessageMap.empty,
    order = None,
    group = None,
    sourceIRI = None
  )

  def emptyPropertyShape(
                          id: RDFNode,
                          path: SHACLPath): PropertyShape = PropertyShape(
    id = id,
    path = path,
    components = List(),
    targets = Seq(),
    propertyShapes = Seq(),
    closed = false,
    ignoredProperties = List(),
    deactivated = false,
    message = MessageMap.empty,
    severity = None,
    name = MessageMap.empty,
    description = MessageMap.empty,
    order = None,
    group = None,
    sourceIRI = None,
    annotations = List()
  )
}
