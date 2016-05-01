package es.weso.validating
import Validated._
import org.scalatest._

class ValidatedTest extends FunSpec with Matchers {

  describe("Validated") {
    
    it("Checks simple ok") {
      val x2: Validated[Int,String,Throwable] = ok(2,"ok")
      x2.isOK should be(true)
    }
    
    it("Checks simple error") {
      val e: Validated[Int,String,Throwable] = errString("Error")
      e.isOK should be(false)
    }
    
    it("Converts an ok value into an error") {
      val v: Validated[Int,String,Throwable] = ok(2,"ok2")
      val e = v.addError("Hi")
      e.isOK should be(false) 
      e.errors should contain only("Hi")
    }
    
    it("Accumulates several errors") {
      val v: Validated[Int,String,Throwable] = ok(2,"ok2")
      val e = v.addErrors(Seq("Hi","man"))
      e.isOK should be(false) 
      e.errors should contain only("Hi", "man")
    }

    it("should be able to fold an ok value") {
      val v: Validated[Int,String,Throwable] = ok(2,"ok")
      val folded = v.fold(x => x.values.map(p => Response(p.value + 1, p.reason)), _ => 0)
      folded should be(Seq(Response(3,"ok")))
    }
    
    it("should be able to fold an errored value") {
      val v: Validated[Int,String,Throwable] = errString("Hi")
      val folded = v.fold(x => x.values.map(p => Response(p.value + 1, p.reason)), _ => 0)
      folded should be(0)
    }

/*    it("should be able to check some conditions when all pass") {
      val isEven : Constraint[Seq[Int],Int,String,Throwable] = (x) => (ctx) => 
        if (x % 2 == 0) ok(x,"ok") else errString("not even")
      val isPositive : Constraint[Seq[Int],Int,String,Throwable] = (x) => (ctx) => 
        if (x > 0) ok(x,"ok") else errString("not positive")
      val c = some(Seq(isEven,isPositive))        
      val validated = 
      checker.isOK should be(true)
    }
    
    it("should be able to check some conditions when some fail") {
      val isEven : Int => Checker[Int,String] = (x) => if (x % 2 == 0) ok(x) else err("not even")
      val isPositive : Int => Checker[Int,String] = (x) => if (x > 0) ok(x) else err("not positive")
      val checker = checkSome(3,Seq(isPositive,isEven),"none")
      checker.isOK should be(true)
    }

    it("should be able to check some conditions when all fail") {
      val isEven : Int => Checker[Int,String] = (x) => if (x % 2 == 0) ok(x) else err("not even")
      val isPositive : Int => Checker[Int,String] = (x) => if (x > 0) ok(x) else err("not positive")
      val checker = checkSome(-3,Seq(isPositive,isEven),"none")
      checker.isOK should be(false)
      checker.errors should contain only("none")
    }
    
    it("should be able to check all conditions when all pass") {
      val isEven : Int => Checker[Int,String] = (x) => if (x % 2 == 0) ok(x) else err("not even")
      val isPositive : Int => Checker[Int,String] = (x) => if (x > 0) ok(x) else err("not positive")
      val checker = checkValueAll(2,Seq(isPositive,isEven))
      checker.isOK should be(true)
    }
    
    it("should be able to check all conditions when some fail") {
      val isEven : Int => Checker[Int,String] = (x) => if (x % 2 == 0) ok(x) else err("not even")
      val isPositive : Int => Checker[Int,String] = (x) => if (x > 0) ok(x) else err("not positive")
      val checker = checkValueAll(3,Seq(isPositive,isEven))
      checker.isOK should be(false)
      checker.errors should contain only("not even")
    }
    
    it("should be able to check all conditions when all fail") {
      val isEven : Int => Checker[Int,String] = (x) => if (x % 2 == 0) ok(x) else err("not even")
      val isPositive : Int => Checker[Int,String] = (x) => if (x > 0) ok(x) else err("not positive")
      val checker = checkValueAll(-3,Seq(isPositive,isEven))
      checker.isOK should be(false)
      checker.errors should contain only("not even", "not positive")
    }
*/
  }

}

//  val x : Validated[Int,String,String] = ok(2,"is 2")
  
  
/*  val positive: Constraint[Int,String,String,Seq[Int]] = (xs) => { (x) =>
    if (xs contains x) { 
      if (x > 0) ok(x, "positive")
      else err("non positive")
    }
    else err("not in context") 
  }
*/
