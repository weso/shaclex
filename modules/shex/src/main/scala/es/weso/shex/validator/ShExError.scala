package es.weso.shex
import cats._
import cats.implicits._
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.implicits.showShEx._

abstract class ShExError


case class StringError(msg: String) extends ShExError {
  override def toString: String =
    ShExError.showViolationError.show(this)
}

case class NotEnoughArcs(node: RDFNode,
                         values: Set[RDFNode],
                         path: Path,
                         min: Int
                        ) extends ShExError {
  override def toString =
    s"""Not enough values for node: ${node.show}
      Path: ${path.toString}
      Values: ${values.map(_.show).mkString(",")}
      Min expected: $min"""
}

case class LabelNotFound(label: ShapeLabel, availableLabels: List[ShapeLabel]) extends ShExError {
  override def toString =
    s"""Label not found: ${label.show}
      Available labels: ${availableLabels.map(_.show).mkString(",")}"""
}

case class NoStart(node: RDFNode) extends ShExError {
  override def toString =
    s"""Checking node ${node.show}@start but no start found"""
}


object ShExError {

  def msgErr(msg: String): ShExError = StringError(msg)

  implicit def showViolationError = new Show[ShExError] {
    override def show(e: ShExError): String = e match {
      case StringError(s) =>  s"Error: $s"
      case _ => e.toString
   }
  }
}

