package es.weso.rbe.interval

import es.weso.collection._
import es.weso.rbe._
import org.scalatest._

class IntervalSingleTest extends FunSpec with BagMatchers {

  describe("Single test") {
    //    noMatchBag(Repeat(Symbol("a", 1, 1), 0, 0), Bag.toBag(List("a")))
    matchBag(Repeat(Symbol("a", 1, 1), 0, 0), Bag.toBag(List("b")))

  }

}
