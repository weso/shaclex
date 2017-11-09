package es.weso.shacl
import cats._

case class Evidences(ls: List[Evidence])

abstract class Evidence {
  override def toString = Evidence.evidenceShow.show(this)
}

case class NodeShapeEvidence(pair: NodeShapePair, msg: String) extends Evidence
case class MsgEvidence(msg: String) extends Evidence

object Evidence {
  implicit val evidenceShow = new Show[Evidence] {
    def show(e: Evidence) = e match {
      case NodeShapeEvidence(pair, msg) => s"${pair}: $msg"
      case MsgEvidence(msg) => msg
    }
  }
}