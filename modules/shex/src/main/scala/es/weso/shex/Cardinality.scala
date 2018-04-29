package es.weso.shex


object Cardinality {
  lazy val defaultMin = 1
  lazy val defaultMax = IntMax(1)

  def isDefault(min: Int, max: Max): Boolean = {
    min == defaultMin && max == defaultMax
  }
}
