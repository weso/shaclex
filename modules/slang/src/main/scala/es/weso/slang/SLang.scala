package es.weso.slang

import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._

sealed trait SLang extends Product with Serializable {
  def children: List[SLang]
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
final case class QualifiedArc(pp: PropPath, s: SLang, card: Card) extends SLang {
  def children = List(s)
  override def toString: String = s"qa($pp,$s,$card)"
}

object SLang {
  def strue: SLang = STrue
  def sfalse: SLang = Not(STrue)
  def or(s1: SLang, s2: SLang): SLang = Not(And(Not(s1), Not(s2)))
  def and(s1: SLang, s2: SLang): SLang = And(s1,s2)
  def sfalse(s1: SLang): SLang        = Not(STrue)
  def string: SLang = Datatype(`xsd:string`)

  def iri(str: String): IRI = IRI(str)
  def zeroStar: Card        = Card(0, Star)
}

