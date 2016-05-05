package es.weso.validating
import Validated._
import Responses._
import org.scalatest._
import cats.implicits._

class ValidatedTest 
 extends FunSpec with Matchers with OptionValues {

  describe("Validated") {
    
    it("Checks simple ok") {
      val x2: Validated[Int,List,Throwable] = ok(List(2))
      x2.isOK should be(true)
    }
    
    it("Checks simple error") {
      val e: Validated[Int,List,Throwable] = errString("Error")
      e.isOK should be(false)
    }
    
    it("Converts an ok value into an error") {
      val v: Validated[Int,List,Throwable] = ok(List(2))
      val e = v.addError("Hi")
      e.isOK should be(false) 
      e.errors should contain only("Hi")
    }
    
    it("Accumulates several errors") {
      val v: Validated[Int,List,Throwable] = ok(List(2))
      val e = v.addErrors(Seq("Hi","man"))
      e.isOK should be(false) 
      e.errors should contain only("Hi", "man")
    }

    it("should be able to fold an ok value") {
      val v: Validated[Int,List,Throwable] = ok(List(2))
      val folded = v.fold(x => x.values.map(r => r.mapValue(_ + 1)), _ => 0)
      folded should be(Seq(Response(List(3))))
    }
    
    it("should be able to fold an errored value") {
      val v: Validated[Int,List,Throwable] = errString("Hi")
      val folded = v.fold(x => x.values.map(r => r.mapValue(_ + 1)), _ => 0)
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
    describe("all") {
      it("should be able to pass when one pass") {
        type Explain[A] = Option[A]
        type E = Throwable
        type ValidatedSingle = Validated[Int,Explain,E]
        type ValidatedSeq = Validated[Seq[Int],Explain,E]
        
        val v1: ValidatedSingle = ok(Some(1))
        val v2: ValidatedSingle = ok(Some(2))
        val vs : Seq[ValidatedSingle] = Seq(v1,v2)
        val vall: ValidatedSeq = Validated.all(vs)
        vall.isOK should be(true)
        vall.errors should be(Seq())
        val reasons = vall.reasons.value
        val expected : ValidatedSeq = ok(Some(Seq(1,2)))
        vall.reasons should be(expected)
      }
      
    }

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
