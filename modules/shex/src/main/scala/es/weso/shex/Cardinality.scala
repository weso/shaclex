package es.weso.shex

case class Cardinality(min: Int, max: Max) {

  def contains(n:Int): Boolean =
    n >= min && max.biggerThanOrEqual(n)

  def isDefault: Boolean = Cardinality.isDefault(min,max)

}

object Cardinality {
  lazy val defaultMin = 1
  lazy val defaultMax = IntMax(1)

  def isDefault(min: Int, max: Max): Boolean = {
    min == defaultMin && max == defaultMax
  }

}
