package es.weso.rbe

import org.scalatest._
import es.weso.collection._
import es.weso.rbe.interval._
import interval._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers._
import es.weso.rbe.deriv._
import org.scalacheck._

class RbeTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  
  describe("Symbols") {
    val rbe = Or(And(Symbol("a",1,3),Symbol("b",1,1)),Symbol("b",2,3))
    rbe.symbols should contain only ("a","b")
  }

  describe("No symbols in bag") {
    val rbe = Or(And(Symbol("a",1,3),Symbol("b",1,1)),Symbol("b",2,3))
    rbe.noSymbolsInBag(Bag("a","c")) should be (false)
  }

}