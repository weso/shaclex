package es.weso.shex

abstract sealed trait Max {
  def show = this match {
    case IntMax(v) => v.toString
    case Star => "*"
  }

  def biggerThanOrEqual(x: Int) = this match {
    case IntMax(v) => v >= x
    case Star => true
  }
}
case object Star extends Max
case class IntMax(v: Int) extends Max
