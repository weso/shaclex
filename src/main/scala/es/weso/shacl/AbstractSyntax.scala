package es.weso.shacl

import es.weso.rdf.nodes._

case class Shape(
    id: Option[IRI],
    scopes: Seq[Scope],
    filters: Seq[Shape],
    component: Seq[Constraint]
)


sealed abstract class Scope


case class ScopeNode(node: IRI) extends Scope
// TODO...add more types of scopes
// case class ScopeClass(cls: IRI) extends Scope
// case class PropertyScope(predicate: IRI) extends Scope

sealed abstract class Constraint

case class PropertyConstraint(
    id:Option[IRI],
    predicate: IRI,
    components: Seq[PCComponent]
) extends Constraint

case class InversePropertyConstraint(
    id:Option[IRI],
    predicate: IRI,
    components: Seq[IPCComponent]
) extends Constraint

case class NodeConstraint(
    id:Option[IRI],
    predicate: IRI,
    components: Seq[NCComponent]
) extends Constraint

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
case object IRIType extends NodeKindType
case object LiteralType extends NodeKindType
case object BNodeType extends NodeKindType
