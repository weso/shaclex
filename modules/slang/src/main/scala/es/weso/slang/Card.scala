package es.weso.slang

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
