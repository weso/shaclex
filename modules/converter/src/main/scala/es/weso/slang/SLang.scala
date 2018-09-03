package es.weso.slang

import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import cats.implicits._

case class SchemaS(lblMap: Map[Label, SLang]) {
  def getLabel(lbl: Label): Option[SLang] = lblMap.get(lbl)
}

sealed trait SLang extends Product with Serializable {
  def children: List[SLang]
}

sealed trait Max                extends Product with Serializable {
  override def toString: String = this match {
    case IntMax(n) => n.toString
    case Star => "*"
  }
}
final case object Star          extends Max
final case class IntMax(n: Int) extends Max

case class Card(min: Int, max: Max) {
  def satisfies(n: Int) = min <= n && (max match {
    case Star => true
    case IntMax(max) => n <= max
  })

  override def toString: String = s"{$min,$max}"
}

object Card {
  def one: Card = Card(1,IntMax(1))
  def oneStar: Card = Card(1,Star)
  def zeroStar: Card = Card(0,Star)
}

sealed trait Label {
  def toRDFNode: RDFNode

  override def toString: String = {
    s"${toRDFNode.show}"
  }
}
case class IRILabel(iri: IRI) extends Label {
  override def toRDFNode = iri
}
case class BNodeLabel(bNode: BNode) extends Label {
  override def toRDFNode = bNode
}

final case object STrue                    extends SLang {
  def children = List()
  override def toString: String = "true"
}
final case class Ref(ref: Label)             extends SLang {
  def children = List()
  override def toString: String = s"@$ref"
}
final case class And(s1: SLang, s2: SLang) extends SLang {
  def children = List(s1,s2)
  override def toString: String = s"$s1 /\\ $s2"
}
final case class Datatype(iri: IRI)        extends SLang {
  def children = List()
  override def toString: String = s"datatype($iri)"
}
final case object IRIKind                  extends SLang {
  def children = List()
  override def toString: String = "iri"
}
final case object BNodeKind                extends SLang {
  def children = List()
  override def toString: String = "bNode"
}
final case class Not(s: SLang)             extends SLang {
  def children = List(s)
  override def toString: String = s"not($s)"

}
final case class QualifiedArc(predSpec: PredSpec, s: SLang, card: Card) extends SLang {
  def children = List(s)
  override def toString: String = s"qa($predSpec,$s,$card)"
}

trait PredSpec extends Product with Serializable {
  private def showSet[A](vs: Set[A]): String = vs.size match {
    case 0 => s"{}"
    case 1 => vs.head.toString
    case _ => s"${vs.map(_.toString).mkString(",")}"
  }

  override def toString = this match {
    case PredSet(ps) => showSet(ps)
    case NoPredSet(ps) => s"no(${showSet(ps)})"
  }

}
case class PredSet(iris: Set[IRI]) extends PredSpec
case class NoPredSet(iris: Set[IRI]) extends PredSpec

object SLang {
  def strue: SLang = STrue
  def sfalse: SLang = Not(STrue)
  def or(s1: SLang, s2: SLang): SLang = Not(And(Not(s1), Not(s2)))
  def and(s1: SLang, s2: SLang): SLang = And(s1,s2)
  def sfalse(s1: SLang): SLang        = Not(STrue)
  def string: SLang = Datatype(xsd_string)

  def iri(str: String): IRI = IRI(str)
  def zeroStar: Card        = Card(0, Star)
}

