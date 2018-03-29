package es.weso.shacl.report

import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.rdf.path.SHACLPath
import es.weso.shacl._

abstract class AbstractResult

case class ValidationResult(focusNode: RDFNode,
                            resultSeverity: Severity,
                            sourceConstraintComponent: IRI,
                            focusPath: Option[SHACLPath],
                            values: Seq[RDFNode],
                            sourceShape: ShapeRef,
                            details: Seq[AbstractResult],
                            message: Seq[LiteralValue]
                           ) extends AbstractResult
