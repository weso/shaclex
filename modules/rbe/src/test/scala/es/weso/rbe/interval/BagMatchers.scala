package es.weso.rbe.interval

import es.weso.collection.Bag
import es.weso.rbe.Rbe
import org.scalatest._

trait BagMatchers extends FunSpec with Matchers {
  def matchBag[A](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should match ${bag}. Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(true)
    }
  }

  def noMatchBag[A](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should not match ${bag}. Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(false)
    }
  }

  def equalInterval[A](rbe: Rbe[A], bag: Bag[A], expected: Interval) = {
    it(s"Interval of ${bag} with ${rbe} should be ${expected}") {
      IntervalChecker.interval(rbe, bag) should be(expected)
    }
  }

  def containsBag[A](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should contain ${bag}. Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(true)
    }
  }

  def notContainsBag[A](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should not contain ${bag}, Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(false)
    }
  }

}