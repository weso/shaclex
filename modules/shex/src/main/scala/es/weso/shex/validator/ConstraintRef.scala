package es.weso.shex.validator

import cats.Show

case class ConstraintRef(n: Int) extends AnyVal {
  override def toString(): String = s"C$n"
}

object ConstraintRef {
  implicit lazy val orderingConstraintRef = new Ordering[ConstraintRef] {
    def compare(c1: ConstraintRef, c2: ConstraintRef): Int = {
      Ordering[Int].compare(c1.n, c2.n)
    }
  }

  implicit lazy val showConstraintRef =
    Show.fromToString[ConstraintRef]

}
