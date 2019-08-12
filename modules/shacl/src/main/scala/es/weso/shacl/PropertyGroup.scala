package es.weso.shacl
import es.weso.rdf.nodes.{DecimalLiteral, RDFNode}

case class PropertyGroup(
                          order: Option[DecimalLiteral],
                          label: Set[RDFNode]
                        )
