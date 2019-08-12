package es.weso.shacl.validator

import cats._
import es.weso.rdf.nodes._
import es.weso.shacl.report.Severity
import es.weso.shacl.{MessageMap, RefNode}

case class AttemptInfo(node: RDFNode,
                       shape: RefNode,
                       messageMap: MessageMap,
                       severity: Severity
                      ) {

  override def toString = AttemptInfo.nodeShapeShow.show(this)

}

object AttemptInfo {
  implicit val nodeShapeShow = new Show[AttemptInfo] {
    def show(ns: AttemptInfo) = s"[${ns.node},${ns.shape.showId}]"
  }
}
