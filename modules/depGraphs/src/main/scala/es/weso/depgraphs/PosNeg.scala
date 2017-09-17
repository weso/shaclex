package es.weso.depgraphs

abstract class PosNeg {
  def change: PosNeg = this match {
    case Neg => Pos
    case Pos => Neg
  }

}
case object Pos extends PosNeg
case object Neg extends PosNeg
