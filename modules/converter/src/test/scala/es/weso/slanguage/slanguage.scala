package es.weso.slanguage
import es.weso.rdf.nodes._

sealed trait SLang extends Product with Serializable

sealed trait Max                extends Product with Serializable
final case object Star          extends Max
final case class IntMax(n: Int) extends Max

case class Card(min: Int, max: Max)
case class Label(iri: IRI)

final case object STrue                    extends SLang
final case class Ref(ref: Label)             extends SLang
final case class And(s1: SLang, s2: SLang) extends SLang
final case class Datatype(iri: IRI)        extends SLang
final case object IRIKind                  extends SLang
final case object BNodeKind                extends SLang
final case class Not(s: SLang)             extends SLang
final case class QualifiedArc(pred: IRI, shape: SLang, card: Card) extends SLang

object SLang {
  def or(s1: SLang, s2: SLang): SLang = Not(And(Not(s1), Not(s2)))
  def sfalse(s1: SLang): SLang        = Not(STrue)

  def iri(str: String): IRI = IRI(str)
  def zeroStar: Card        = Card(0, Star)
}

