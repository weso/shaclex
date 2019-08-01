package es.weso.shex
import cats._
import cats.implicits._
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.implicits.showShEx._
import es.weso.shex.validator.Attempt

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

case class ErrCardinality(attempt: Attempt, node: RDFNode, path: Path, values: Int, card: Cardinality) extends ShExError {
  override def toString =
    s"""${attempt.show}: #values for ${path.show}=$values doesn't match ${card.show}"""
}

case class ErrCardinalityWithExtra(attempt: Attempt, node: RDFNode, path: Path, values: Int, valuesFailed: Int, card: Cardinality) extends ShExError {
  override def toString =
    s"""${attempt.show}: #values for ${path.show}=$values doesn't match ${card.show}
       | #values that failed: ${valuesFailed}""".stripMargin
}

case class ValuesNotPassed(attempt: Attempt, node: RDFNode, path: Path, valuesPassed: Int, valuesFailed: Set[(RDFNode, String)]) extends ShExError {
  override def toString =
    s"""${attempt.show}: #values for ${path.show} failed}
       | #values that failed: ${showValues(valuesFailed)}""".stripMargin

  private def showValues(vs: Set[(RDFNode, String)]): String = {
    vs.map(pair => s"${pair._1.show}: ${pair._2}").mkString("\n")
  }
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

