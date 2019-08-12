package es.weso.utils
import simulacrum._

@typeclass trait Read[A] {
  def read(x: String): A
}