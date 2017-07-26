package es.weso.shapeMaps

import es.weso.rdf.nodes._
import cats._
import cats.data._
import cats.implicits._

case class ShapeMap(associations: List[Association])

case class Association(
 nodeSelector: NodeSelector,
 shapeLabel: ShapeLabel,
 status: Status = Conformant,
 reason: String = "",
 addInfo: String = ""
)

abstract sealed class Status
case object Conformant extends Status
case object NonConformant extends Status

abstract class ShapeLabel
case class IRILabel(iri: IRI) extends ShapeLabel
case object Start extends ShapeLabel

abstract class NodeSelector
case class RDFNodeSelector(node: RDFNode) extends NodeSelector
case class TriplePattern(subjectPattern: Pattern, predicate: IRI, objectPattern: Pattern) extends NodeSelector
sealed abstract class Pattern
case class IRIPattern(iri: IRI) extends Pattern
case object WildCard extends Pattern
case object Focus extends Pattern


