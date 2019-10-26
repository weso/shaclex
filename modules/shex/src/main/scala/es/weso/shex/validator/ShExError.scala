package es.weso.shex.validator
import cats._
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.shex.implicits.showShEx._
import es.weso.shex._

abstract class ShExError {
  def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String
}


case class StringError(msg: String) extends ShExError {
  override def toString: String =
    ShExError.showViolationError.show(this)

  override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    s"Error: $msg"
  }
}

case class NotEnoughArcs(node: RDFNode,
                         values: Set[RDFNode],
                         path: Path,
                         min: Int
                        ) extends ShExError {
  override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    s"""Not enough values for node: ${nodesPrefixMap.qualify(node)}
      Path: ${path.showQualified(shapesPrefixMap)}
      Values: ${values.map(nodesPrefixMap.qualify).mkString(",")}
      Min expected: $min"""
  }
}

case class LabelNotFound(label: ShapeLabel, availableLabels: List[ShapeLabel]) extends ShExError {
  override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    s"""Label not found: ${shapesPrefixMap.qualify(label.toRDFNode)}
      Available labels: ${availableLabels.map(label => shapesPrefixMap.qualify(label.toRDFNode)).mkString(",")}"""
  }
}

case class NoStart(node: RDFNode) extends ShExError {
  override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    s"""Checking node ${nodesPrefixMap.qualify(node)}@start but no start found"""
  }
}

case class ErrCardinality(attempt: Attempt, node: RDFNode, path: Path, values: Int, card: Cardinality) extends ShExError {
  override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    s"""${attempt.showQualified(nodesPrefixMap,shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)}=$values doesn't match ${card.show}"""
  }
}

case class ErrCardinalityWithExtra(attempt: Attempt, node: RDFNode, path: Path, values: Int, valuesFailed: Int, card: Cardinality) extends ShExError {
  override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    s"""${attempt.showQualified(nodesPrefixMap, shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)}=$values doesn't match ${card.show}
       | #of values that failed: $valuesFailed
       | """.stripMargin
  }
}

case class ValuesNotPassed(attempt: Attempt, node: RDFNode, path: Path, valuesPassed: Int, valuesFailed: Set[(RDFNode, String)]) extends ShExError {
  override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    s"""${attempt.showQualified(nodesPrefixMap, shapesPrefixMap)}: # of values for ${path.showQualified(shapesPrefixMap)} failed}
       | #values that failed: ${showValues(valuesFailed, nodesPrefixMap)}""".stripMargin
  }

  private def showValues(vs: Set[(RDFNode, String)], prefixMap: PrefixMap): String = {
    vs.map(pair => s"${prefixMap.qualify(pair._1)}: ${pair._2}").mkString("\n")
  }

}

case class ClosedButExtraPreds(preds: Set[IRI]) extends ShExError {
  override def showQualified(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    s"""Closed shape but extra properties found: ${preds.map(shapesPrefixMap.qualifyIRI).mkString(",")}"""
  }
}


object ShExError {

  def msgErr(msg: String): ShExError = StringError(msg)

  implicit def showViolationError: Show[ShExError] = new Show[ShExError] {
    override def show(e: ShExError): String = e match {
      case StringError(s) =>  s"Error: $s"
      case _ => e.toString
   }
  }
}

