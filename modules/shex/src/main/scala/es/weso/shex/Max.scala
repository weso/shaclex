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

  def decreaseCard: Max = this match {
    case Star => Star
    case IntMax(0) => IntMax(0)
    case IntMax(n) if n > 0 => IntMax(n - 1)
    case n => throw new Exception(s"DecreaseCard: Unexpected negative valu of max card: $n")
  }

}

case object Star extends Max
case class IntMax(v: Int) extends Max
