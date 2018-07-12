package es.weso.shacl

import es.weso.rdf.nodes.{IRI, Literal, RDFNode}

sealed abstract class Component

case class ClassComponent(value: RDFNode) extends Component {
  println(s"ClassComponent($value(")
}
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

