package es.weso.validating
import cats._
import cats.implicits._
import Constraint._
import CheckedSub._
import ConstraintReason._
import org.scalatest._

/*
class ConstraintTest extends FunSpec with Matchers with OptionValues {

  describe("Constraint") {

    // Several constraints
    val isEven: Constraint[Seq[Int], Int, Option[Int]] = single((x,ctx,r) =>
      if (x % 2 == 0) okSingle(x,Some(x),"even") 
      else errString(x,"not even"))

    val isPos: Constraint[Seq[Int], Int,Option[Int]] = single((x,ctx,r) =>
      if (x > 0) okSingle(x, Some(x), "ok pos") 
      else errString(x,"not positive"))

    val isBiggest: Constraint[Seq[Int], Int, Option[Int]] = single((x,ctx,r) => {
      val biggest = ctx.max
      if (x == biggest) okSingle(x, None, "biggest")
      else errString(x, s"$x is not the biggest")
    })

    describe("check a constraint") {
      it("should be able to check biggest ok") {
        val check = isBiggest.validate(4,Seq(2, 3, 4),None)
        check.isOK should be(true)
      }

      it("should be able to check biggest failing") {
        val check = isBiggest.validate(3,Seq(2, 3, 4),None)
        check.isOK should be(false)
      }
    }
/*
    describe("some") {

      it("should be able to check some when all pass") {
        val c = SomeOf(Seq(isEven, isPositive))
        val validated = c.validate(2,Seq())
        validated.isOK should be(true)
/*        validated.reasons.value.values should contain only (
            Response(SingleReason(2, "ok even")), Response(SingleReason(2, "ok pos"))) */
      }

      it("should be able to validate some when some pass") {
        val c = SomeOf(Seq(isEven, isPositive))
        val validated = c.validate(3,Seq())
        validated.isOK should be(true)
//        validated.reasons.value.values should contain only (Response(SingleReason(3, "ok pos")))
      }

      it("should be able to validate some when all fail failing") {
        val c = SomeOf(Seq(isEven, isPositive))
        val validated = c.validate(-3,Seq())
        validated.isOK should be(false)
      }

    }

    describe("oneOf") {
      it("should be able to fail oneOf when all pass") {
        val c = OneOf(Seq(isEven, isPositive))
        val validated = c.validate(2,Seq())
        validated.isOK should be(false)
        validated.errors.size should be(1)
        val e = validated.errors.head
        e shouldBe a [OneOf_MoreThanOneValid[_]]
      }

      it("should be able to validate oneOf when only one pass") {
        val c = OneOf(Seq(isEven, isPositive))
        val validated = c.validate(3,Seq())
        validated.isOK should be(true)
        val r: ConstraintReason[Int] = OneOfReason(SingleReason(3,"ok pos"))
        validated.reasons.value.values should contain only (
            Response(r))
      }

      it("should be able to fail validation of oneOf when none pass") {
        val c = OneOf(Seq(isEven, isPositive))
        val validated = c.validate(-3,Seq())
        validated.isOK should be(false)
        validated.errors.size should be(1)
        val e = validated.errors.head
        e shouldBe a [OneOf_NoOneValid[_]]
      }

    }
*/
    describe("all") {
      it("should be able to check all when all pass") {
        val c = All(Seq(isEven, isPos))
        val validated = c.validate(2,Seq(),None)
        validated.isOK should be(true)
        val r1: ConstraintReason[Int] = SingleReason("ok even")
        val r2: ConstraintReason[Int] = SingleReason("ok pos")
        val vs = validated.responses
        val expected: Option[Int] = Some(4)
        vs should contain only(expected)
      }

      it("should be able to fail to validate all when only one pass") {
        val c = All(Seq(isEven, isPos))
        val validated = c.validate(3,Seq(),None)
        validated.isOK should be(false)
        val e = validated.errors.head
        e shouldBe a [All_SomeNotValid[_]]
      }

      it("should be able to fail validation of all when none pass") {
        val c = All(Seq(isEven, isPos))
        val validated = c.validate(-3,Seq(),None)
        validated.isOK should be(false)
        val e = validated.errors.head
        e shouldBe a [All_SomeNotValid[_]]
      }

    }
    
  describe("or") {
    val c = OrConstraint(isEven,isPos)
    
    it("Should be able to pass when both pass") {
      val validated = c.validate(2,Seq(),None)
      validated.isOK should be(true)
      // val reasons = validated.reasons.value
      // Add test to check that it starts by SomeReason...
    }
  }

  }
  
  describe("custom errors") {
/*    abstract class C[Context, A, E <: ConstraintError]
    case class Sng[Context, A, E <: ConstraintError](msg: String) extends C[Context,A,E]
    
    case class MyError(msg:String) extends ConstraintError("MyError" + msg)
    type MyConstraint = C[Seq[Int],Int,MyError]
    val myEven: MyConstraint = Sng("hi") */
        
    it("Should be able define a constraint with a custom error") {
//      val validated = myEven.validate(2,Seq())
      //validated.isOK should be(true)
      //val reasons = validated.reasons.value
      // Add test to check that it starts by SomeReason...
    }
  }

  val even : Int => CheckedVal[Int,Option[Int]] = cond(_ % 2 == 0, Some(1), "even")
  val odd : Int => CheckedVal[Int,Option[Int]] = cond(_ % 2 != 0, Some(1), "odd")
  val positive : Int => CheckedVal[Int,Option[Int]] = cond(_ > 0, Some(1), "positive")
  val negative : Int => CheckedVal[Int,Option[Int]] = cond(_ < 0, Some(1), "negative")

  describe("checkValueAll") {
    it("checkValueAll should pass 3 with odd and positive") {
      Constraint.checkAll(3, Seq(odd, positive)).isOK should be(true)
    }
    it("checkValueAll should not pass 3 with odd and negative") {
      Constraint.checkAll(3, Seq(odd, negative)).isOK should be(false)
    }
    it("checkValueAll should not pass 3 with even and positive") {
      Constraint.checkAll(3, Seq(even, positive)).isOK should be(false)
    }
    it("checkValueAll should not pass 3 with even and negative") {
      Constraint.checkAll(3, Seq(even, negative)).isOK should be(false)
    }

    it("should allow different errors") {
//      case class MyError(msg:String) extends ConstraintError(msg)

//      val myEven : Int => Checked[Int,ConstraintReason,MyError] = cond(_ % 2 == 0, "even")
    }
  }

  describe("checkSome") {

    it("should pass 3 with odd and positive") {
      checkSome(3, Seq(odd, positive)).isOK should be(true)
    }
    it("should pass 3 with odd and negative") {
      checkSome(3, Seq(odd, negative)).isOK should be(true)
    }
    it("should pass 3 with even and positive") {
      checkSome(3, Seq(even, positive)).isOK should be(true)
    }
    it("should not pass 3 with even and negative") {
      Constraint.checkAll(3, Seq(even, negative)).isOK should be(false)
    } 
  } 
  
  
  describe("Combine all") {
    it("should check if a list of integers is positive when all are positive") {
     val vs: Seq[Int] = Seq(1,2,3)
     val current: Option[Int] = Some(0)
     val comb: (Int,Option[Int]) => CheckedVal[String,Option[Int]] = { (x,o) =>
       if (x >= 0) {
         okSingle(x.toString, Some(x),s"$x is positive")
       } else 
         errString(x.toString,s"$x is negative")
     }
    
     val result : CheckedVal[String,Option[Int]] = combineAll(vs,current,comb)
     result.isOK should be(true)
     result.responses should contain only(Some(6))
    }

    it("should check if a list of integers is positive when one is negative") {
     val vs: Seq[Int] = Seq(1,-2,3)
     val current: Option[Int] = Some(0)
     val comb: (Int,Option[Int]) => CheckedVal[String,Option[Int]] = { (x,o) =>
       if (x >= 0) {
         okSingle(x.toString, Some(x),s"$x is positive")
       } else 
         errString(x.toString,s"$x is negative")
     }
    
     val result : CheckedVal[String,Option[Int]] = combineAll(vs,current,comb)
     result.isOK should be(false)
     result.responses shouldBe empty
    }
  }
  
}

*/
