package es.weso.shapeMaps

import es.weso.rdf.nodes._
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.RDFReader

case class ShapeMap(associations: List[Association]) {

  def addAssociation(a: Association): ShapeMap = ShapeMap(a +: associations)

  /**
    * Resolve triple patterns according to an RDF
    */
  def fixShapeMap(rdf: RDFReader): Either[String, ShapeMap] = {
    val empty: Either[String,ShapeMap] = Right(ShapeMap.empty)
    def combine(a: Association, current: Either[String,ShapeMap]): Either[String,ShapeMap] = {
      a.nodeSelector match {
        case RDFNodeSelector(node) => for {
          sm <- current
        } yield sm.addAssociation(a)
        case TriplePattern(Focus,p,o) => {
          o match {
            case WildCard => ???
            case NodePattern(obj) => ???
            case Focus => Left(s"FixShapeMap: Inconsistent triple pattern in node selector with two Focus: ${a.nodeSelector}")
          }
        }
        case TriplePattern(s,p,Focus) => ???
        case _ => Left(s"FixShapeMap: Inconsistent triple pattern in node selector ${a.nodeSelector}")
      }
    }
    associations.foldRight(empty)(combine)
  }
}

object ShapeMap {
  def empty = ShapeMap(List())
}

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
case class  NodePattern(node: RDFNode) extends Pattern
case object WildCard extends Pattern
case object Focus extends Pattern


