package es.weso.collection

import scala.collection.SortedMap

import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers

class SortedMapBagTest extends FunSpec with Matchers with Checkers {

  describe("A SortedMap Bag") {
    it("Should add one element and have multiplicity 1") {
      val sm :SortedMap[Char,Int] = SortedMap[Char,Int]()
      val bag = BagSortedMap[Char](sm)
      val expected = SortedMap[Char,Int]('b' -> 3)
      bag.add('b',3).asSortedMap should be(expected)
    }


  }

}