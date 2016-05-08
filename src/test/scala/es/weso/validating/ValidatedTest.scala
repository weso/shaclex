package es.weso.validating
import Validated._
import Responses._
import org.scalatest._
import cats.implicits._

class ValidatedTest 
 extends FunSpec with Matchers with OptionValues {
  
   type Explain[A] = Option[A]
   type E = Throwable
   type ValidatedSingle = Validated[Int,Explain,E]
   type ValidatedSeq = Validated[Seq[Int],Explain,E]

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

    it("should be able to merge a value with a list") {
      val v1: Validated[Int,List,Throwable] = ok(List(1))
      val v23: Validated[Seq[Int],List,Throwable] = ok(List(Seq(2,3)))
      val v = v1.merge(v23) 
      v.isOK should be(true)
      val expected: Responses[Seq[Int],List] = Responses(Seq(Response(List(Seq(1,2,3)))))
      val reasons = v.reasons.value
      reasons should be(expected)
    }

    it("should be able to merge an empty value with a list") {
      val v0: Validated[Int,List,Throwable] = okZero()
      val v23: Validated[Seq[Int],List,Throwable] = ok(List(Seq(2,3)))
      val v = v0.merge(v23) 
      v.isOK should be(true)
      val expected: Responses[Seq[Int],List] = Responses(Seq(Response(List(Seq(2,3)))))
      val reasons = v.reasons.value
      reasons should be(expected)
    }
    
    describe("all") {
      it("should be able to pass when one pass") {
        val v1: ValidatedSingle = ok(Some(1))
        val v2: ValidatedSingle = ok(Some(2))
        val vs : Seq[ValidatedSingle] = Seq(v1,v2)
        val vall: ValidatedSeq = Validated.all(vs)
        vall.isOK should be(true)
        vall.errors should be(Seq())
        val reasons = vall.reasons.value
        val expected : Responses[Seq[Int],Explain] = 
          Responses(Seq(Response(Some(Seq(1,2)))))
        reasons should be(expected)
      }
      
      it("should fail when one fails") {
        val v1: ValidatedSingle = errString("err")
        val v2: ValidatedSingle = ok(Some(2))
        val vs : Seq[ValidatedSingle] = Seq(v1,v2)
        val vall: ValidatedSeq = Validated.all(vs)
        vall.isOK should be(false)
      }
    }

  }
}


