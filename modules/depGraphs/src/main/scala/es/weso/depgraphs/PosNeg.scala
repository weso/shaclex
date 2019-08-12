package es.weso.depgraphs

sealed trait PosNeg {
  def change: PosNeg = this match {
    case Neg => Pos
    case Pos => Neg
    case Both => Both
  }
  def combine(other: PosNeg): PosNeg
}

case object Pos extends PosNeg {
  override def combine(other: PosNeg): PosNeg = other match {
    case Pos => Pos
    case Neg => Both
    case Both => Both
  }
}
case object Neg extends PosNeg {
  override def combine(other: PosNeg): PosNeg = other match {
    case Pos => Both
    case Neg => Neg
    case Both => Both
  }
}
case object Both extends PosNeg {
  override def combine(other: PosNeg): PosNeg = Both
}
