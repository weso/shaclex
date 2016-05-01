package es.weso.generic

trait Monoid[A] {
  def combine(a1:A, a2: A): A
  def empty: A
}

object Monoid {

val stringMonoid = new Monoid[String] {
  def combine(a1: String, a2: String): String = a1 + a2
  def empty = ""
}

}