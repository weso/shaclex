package es.weso.shacl.validator

import cats._
import es.weso.rdf.nodes.RDFNode
import es.weso.shacl.RefNode

case class Evidences(ls: List[Evidence])

abstract class Evidence {
  override def toString = Evidence.evidenceShow.show(this)
}

case class NodeShapeEvidence(node: RDFNode,
                             shape: RefNode,
                             msg: String
                            ) extends Evidence
case class MsgEvidence(msg: String) extends Evidence

object Evidence {
  implicit val evidenceShow = new Show[Evidence] {
    def show(e: Evidence) = e match {
      case NodeShapeEvidence(node, shape, msg) => s"$node@${shape.id}: $msg"
      case MsgEvidence(msg) => msg
    }
  }
}