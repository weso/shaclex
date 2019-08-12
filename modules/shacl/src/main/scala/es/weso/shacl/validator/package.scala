package es.weso.shacl

import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.path.SHACLPath
import es.weso.shacl.validator.SHACLChecker.Check

package object validator {
  type Result = (ShapeTyping, Boolean)
  type CheckTyping = Check[Result]
  type PropertyChecker = (Attempt, SHACLPath) => CheckTyping
  type NodeChecker = Attempt => RDFNode => CheckTyping
  type ShapeChecker = Shape => CheckTyping
  type NodeShapeChecker = (RDFNode, Shape) => CheckTyping

}
