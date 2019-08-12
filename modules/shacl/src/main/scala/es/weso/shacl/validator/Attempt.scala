package es.weso.shacl.validator

import es.weso.rdf.nodes._
import es.weso.rdf.path.SHACLPath
import es.weso.shacl.{MessageMap, RefNode}
import es.weso.shacl.report.Severity
/**
 * Represents current validation attempt
 * It contains the node and a shape
 * It may contain a predicate, path or nothing
 */
case class Attempt(node: RDFNode,
                   shapeRef: RefNode,
                   messageMap: MessageMap,
                   severity: Severity,
                   path: Option[SHACLPath]
                  ) {
  def shapeId: RDFNode = shapeRef.id
}

