package es.weso.shex.validator
import cats._, data._
import implicits._

case class Evidences(ls: List[(NodeShape, String)]) {
  def addEvidence(ns: NodeShape, msg: String): Evidences = {
    Evidences((ns, msg) :: ls)
  }

  override def toString: String =
    ls.map { case (ns, msg) => s"${ns.node} - ${ns.shape}: $msg" }.mkString("\n")
}

object Evidences {
  def initial = Evidences(List())

  implicit def showEvidences = new Show[Evidences] {
    // TODO: Improve this...
    def show(e: Evidences): String = {
      e.ls.toString
    }
  }

}

