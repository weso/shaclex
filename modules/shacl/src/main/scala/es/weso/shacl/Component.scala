package es.weso.shacl

import es.weso.rdf.nodes.{IRI, Literal, RDFNode}

sealed abstract class Component {
  val name: String
}

case class ClassComponent(value: RDFNode) extends Component {
  override val name: String = "class"
}
case class Datatype(value: IRI) extends Component {
  override val name: String = "datatype"
}
case class NodeKind(value: NodeKindType) extends Component {
  override val name: String = "nodeKind"
}
case class MinCount(value: Int) extends Component {
  override val name: String = "minCount"
}
case class MaxCount(value: Int) extends Component {
  override val name: String = "maxCount"
}
case class MinExclusive(value: Literal) extends Component {
  override val name: String = "minExclusive"
}
case class MinInclusive(value: Literal) extends Component {
  override val name: String = "minInclusive"
}
case class MaxExclusive(value: Literal) extends Component {
  override val name: String = "maxExclusive"
}
case class MaxInclusive(value: Literal) extends Component {
  override val name: String = "maxInclusive"
}
case class MinLength(value: Int) extends Component {
  override val name: String = "minLength"
}
case class MaxLength(value: Int) extends Component {
  override val name: String = "maxLength"
}
case class Pattern(pattern: String, flags: Option[String]) extends Component {
  override val name: String = "pattern"
}
case class UniqueLang(value: Boolean) extends Component {
  override val name: String = "uniqueLang"
}
case class LanguageIn(langs: List[String]) extends Component {
  override val name: String = "languageIn"
}
case class Equals(p: IRI) extends Component {
  override val name: String = "equals"
}
case class Disjoint(p: IRI) extends Component {
  override val name: String = "disjoint"
}
case class LessThan(p: IRI) extends Component {
  override val name: String = "lessThan"
}
case class LessThanOrEquals(p: IRI) extends Component {
  override val name: String = "lessThanOrEquals"
}
case class Or(shapes: List[RefNode]) extends Component {
  override val name: String = "or"
}
case class And(shapes: List[RefNode]) extends Component {
  override val name: String = "and"
}
case class Not(shape: RefNode) extends Component {
  override val name: String = "not"
}
case class Xone(shapes: List[RefNode]) extends Component {
  override val name: String = "xone"
}
case class Closed(isClosed: Boolean, ignoredProperties: List[IRI]) extends Component {
  override val name: String = "closed"
}
case class NodeComponent(shape: RefNode) extends Component {
  override val name: String = "node"
}
case class HasValue(value: Value) extends Component {
  override val name: String = "hasValue"
}
case class In(list: List[Value]) extends Component {
  override val name: String = "in"
}

// TODO: Change representation to include optional parent shape
case class QualifiedValueShape(
                                shape: RefNode,
                                qualifiedMinCount: Option[Int],
                                qualifiedMaxCount: Option[Int],
                                qualifiedValueShapesDisjoint: Option[Boolean]) extends Component {
  override val name: String = "qualifiedValueShape"
}

