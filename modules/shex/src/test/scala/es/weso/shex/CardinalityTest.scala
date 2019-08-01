package es.weso.shex
import org.scalatest._

class CardinalityTest extends FunSpec with Matchers with EitherValues {

  describe(s"Cardinality test") {
    checkBetween(2,1,IntMax(5))
    checkBetween(2,0,IntMax(2))
    checkBetween(2,2,IntMax(5))
    checkBetween(2,2,Star)
    checkBetween(3,2,Star)
    checkBetween(0,0,Star)
    checkBetween(1000,0,Star)
    checkNotBetween(0,1,Star)
    checkNotBetween(10,11,Star)
    checkNotBetween(10,42,Star)
  }

  def checkBetween(n: Int, min: Int, max: Max): Unit = {
    it(s"Should check that $n is between $min and $max") {
      Cardinality(min,max).contains(n) should be(true)
    }
  }
  def checkNotBetween(n: Int, min: Int, max: Max): Unit = {
    it(s"Should check that $n is not between $min and $max") {
      Cardinality(min,max).contains(n) should be(false)
    }
  }
}
