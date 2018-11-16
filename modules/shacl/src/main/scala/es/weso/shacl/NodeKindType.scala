package es.weso.shacl

import es.weso.rdf.nodes.IRI
import es.weso.shacl.SHACLPrefixes._

sealed trait NodeKindType {
  def id: IRI
}
case object IRIKind extends NodeKindType {
  override def id = `sh:IRI`
}
case object LiteralKind extends NodeKindType {
  override def id = `sh:Literal`
}
case object BlankNodeKind extends NodeKindType {
  override def id = `sh:BlankNode`
}
case object BlankNodeOrIRI extends NodeKindType {
  override def id = `sh:BlankNodeOrIRI`
}
case object BlankNodeOrLiteral extends NodeKindType {
  override def id = `sh:BlankNodeOrLiteral`
}
case object IRIOrLiteral extends NodeKindType {
  override def id = `sh:IRIOrLiteral`
}
