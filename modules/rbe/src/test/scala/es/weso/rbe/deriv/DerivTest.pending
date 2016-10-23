package es.weso.rbe.deriv
import es.weso.rbe._
import org.scalatest._
import es.weso.collection._

class DerivTest extends FunSpec with Matchers with TryValues {

  describe("Calculate derivative of symbol") {
    shouldCheck(Or(Symbol("a",0,1), Symbol("b",0,1)),Bag("a"),true)
    shouldCheck(Or(Symbol("a",0,1), Symbol("b",0,1)),Bag("b"),true)
    shouldNotCheck(Or(Symbol("a",0,1), Symbol("b",0,1)),Bag("a","b"),true)
    shouldNotCheck(Or(Symbol("a",0,1), Symbol("b",0,1)),Bag("a","b"),false)
    shouldCheck(And(Symbol("a",0,1), Symbol("b",0,1)),Bag("a","b"),true)
    shouldCheck(Symbol("a",0,1),Bag("a"),true)
    shouldCheck(Symbol("a",0,1),Bag("b"),true)
    shouldNotCheck(Symbol("a",0,1),Bag("b"),false)
    shouldNotCheck(Symbol("a",0,1),Bag("a","b"),false)
    shouldNotCheck(Symbol("a",0,1),Bag("a","a"),true)
    shouldNotCheck(Symbol("a",1,1),Bag("a","a"),true)
    shouldNotCheck(Symbol("a",1,1),Bag("a","a"),false)
    shouldCheck(Symbol("a",1,1),Bag("a","b"),true)
    shouldNotCheck(Symbol("a",1,1),Bag("a","b"),false)
  }
  
  def shouldCheck[A](rbe: Rbe[A], bag: Bag[A], open: Boolean = true): Unit = {
    it(s"$rbe ~  $bag ($open)") {
      val derivMatcher = DerivChecker(rbe)
      val d = derivMatcher.check(bag,open)
      if (!d.isOK) {
        fail(s"Error: $d")
      }
    }
  }
  
    def shouldNotCheck[A](rbe: Rbe[A], bag: Bag[A], open: Boolean = true): Unit = {
    it(s"$rbe !~ $bag ($open)") {
      val derivMatcher = DerivChecker(rbe)
      val d = derivMatcher.check(bag,open)
      if (d.isOK) {
        fail(s"Check but should not. Deriv = $d")
      }
    }
  }

}