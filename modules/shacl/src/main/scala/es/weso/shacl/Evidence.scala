package es.weso.shacl
import cats._, data._
import cats.implicits._
import NodeShapePair._

case class Evidences(ls: List[Evidence])

abstract class Evidence {
  def toString(e: Evidence): String = {
    e match {
      case NodeShapeEvidence(pair,msg) => s"$msg - ${pair}"
      case MsgEvidence(msg) => msg
    }
  }
//  override def toString = Show[Evidence].show(this)
}

case class NodeShapeEvidence(pair: NodeShapePair, msg: String) extends Evidence
case class MsgEvidence(msg: String) extends Evidence

