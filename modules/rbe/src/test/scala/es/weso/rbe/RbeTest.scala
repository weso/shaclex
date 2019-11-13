package es.weso.rbe

import org.scalatest._
import es.weso.collection._
import org.scalatestplus.scalacheck._

class RbeTest extends FunSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  describe("Symbols") {
    val rbe = Or(And(Symbol("a", 1, 3), Symbol("b", 1, 1)), Symbol("b", 2, 3))
    rbe.symbols should contain only ("a", "b")
  }

  describe("No symbols in bag") {
    val rbe = Or(And(Symbol("a", 1, 3), Symbol("b", 1, 1)), Symbol("b", 2, 3))
    rbe.noSymbolsInBag(Bag("a", "c")) should be(false)
  }

}