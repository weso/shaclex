package es.weso.collection

import org.scalatest._
import org.scalatestplus.scalacheck._

class BagTest
  extends FunSpec
  with Matchers
  with Checkers {

  describe("A Bag") {

    it("Should add one element and have multiplicity 1") {
      val emptyBag: Bag[Char] = Bag.empty
      emptyBag.insert('a').multiplicity('a') should be(1)
    }

    it("Should add one element twice and have multiplicity 2") {
      val emptyBag: Bag[Char] = Bag.empty
      emptyBag.insert('a').insert('a').multiplicity('a') should be(2)
    }

    it("Should add one element twice, remove it and have multiplicity 1") {
      val emptyBag: Bag[Char] = Bag.empty
      emptyBag.insert('a').insert('a').delete('a').multiplicity('a') should be(1)
    }

    it("Should add two elements and have size 2") {
      val emptyBag: Bag[Char] = Bag.empty
      emptyBag.insert('a').insert('b').elems.size should be(2)
    }

    it("Should add one element three times and have multiplicity 3") {
      val emptyBag: Bag[Char] = Bag.empty
      emptyBag.add('a', 3).multiplicity('a') should be(3)
    }

    it("Should add one element twice and have size 1") {
      val emptyBag: Bag[Char] = Bag.empty
      emptyBag.insert('a').insert('a').elems.size should be(1)
    }

    it("should calculate delta") {
      val bag = Bag.toBag(List(1, 1, 2, 2, 3))
      val delta = Bag.delta(Seq(1, 2), bag)
      val expected = Bag.toBag(List(1, 1, 2, 2))
      delta should be(expected)
    }

    /*  TODO: I have removed the following checkers because there seem to be an
    incompatibility problem with the scalacheck library and scalatest...
     I need to see where is that problem...

    it("converting a bag from a list of chars has the same size as the list") {
      check((ls: List[Char]) => {
        val bag = Bag.toBag(ls)
        bag.size == ls.size
      }
      )
    }

    it("converting a bag from a list of ints has the same size as the list") {
      check((ls: List[Int]) => {
        val bag = Bag.toBag(ls)
        bag.size == ls.size
      }
      )
    }

    it("count of different numbers is equal to number of elements") {
      check((ls: List[Int]) => {
        val set: Set[Int] = ls.toSet
        val bag = Bag.toBag(ls)
        bag.elems.toList.size == set.size
      }
      )
    }

*/
  }

}
