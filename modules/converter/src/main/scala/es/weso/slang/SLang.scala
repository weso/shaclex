package es.weso.slang
import es.weso.rdf.nodes._

case class SchemaS(lblMap: Map[Label, SLang]) {
  def getLabel(lbl: Label): Option[SLang] = lblMap.get(lbl)
}

sealed trait SLang extends Product with Serializable {
  def children: List[SLang]
}

sealed trait Max                extends Product with Serializable
final case object Star          extends Max
final case class IntMax(n: Int) extends Max

case class Card(min: Int, max: Max) {
  def satisfies(n: Int) = min <= n && (max match {
    case Star => true
    case IntMax(max) => n <= max
  })
}

object Card {
  def one: Card = Card(1,IntMax(1))
  def oneStar: Card = Card(1,Star)
  def zeroStar: Card = Card(0,Star)
}

sealed trait Label {
  def toRDFNode: RDFNode
}
case class IRILabel(iri: IRI) extends Label {
  override def toRDFNode = iri
}
case class BNodeLabel(bNode: BNode) extends Label {
  override def toRDFNode = bNode
}

final case object STrue                    extends SLang {
  def children = List()
}
final case class Ref(ref: Label)             extends SLang {
  def children = List()
}
final case class And(s1: SLang, s2: SLang) extends SLang {
  def children = List(s1,s2)
}
final case class Datatype(iri: IRI)        extends SLang {
  def children = List()
}
final case object IRIKind                  extends SLang {
  def children = List()
}
final case object BNodeKind                extends SLang {
  def children = List()
}
final case class Not(s: SLang)             extends SLang {
  def children = List(s)
}
final case class QualifiedArc(pred: PredSpec, s: SLang, card: Card) extends SLang {
  def children = List(s)
}

trait PredSpec extends Product with Serializable
case class PredSet(iris: Set[IRI]) extends PredSpec
case class NoPredSet(iris: Set[IRI]) extends PredSpec

object SLang {
  def strue: SLang = STrue
  def sfalse: SLang = Not(STrue)
  def or(s1: SLang, s2: SLang): SLang = Not(And(Not(s1), Not(s2)))
  def and(s1: SLang, s2: SLang): SLang = And(s1,s2)
  def sfalse(s1: SLang): SLang        = Not(STrue)

  def iri(str: String): IRI = IRI(str)
  def zeroStar: Card        = Card(0, Star)
}

