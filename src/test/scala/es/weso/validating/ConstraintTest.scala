package es.weso.validating
import Constraint._
import Validated._
import org.scalatest._
import cats.implicits._

class ConstraintTest extends FunSpec with Matchers with OptionValues {

  describe("Constraint") {

    // Several constraints
    val isEven: Constraint[Seq[Int], Int, Throwable] = Single((x,ctx) =>
      if (x % 2 == 0) ok(SingleReason(x, "ok even")) else errString("not even"))

    val isPositive: Constraint[Seq[Int], Int, Throwable] = Single((x,ctx) =>
      if (x > 0) ok(SingleReason(x, "ok pos")) else errString("not positive"))

    val isBiggest: Constraint[Seq[Int], Int, Throwable] = Single((x,ctx) => {
      val biggest = ctx.max
      if (x == biggest) ok(SingleReason(x, "biggest"))
      else errString(s"$x is not the biggest")
    })

    describe("check a constraint") {
      it("should be able to check biggest ok") {
        val check = isBiggest.validate(4,Seq(2, 3, 4))
        check.isOK should be(true)
      }

      it("should be able to check biggest failing") {
        val check = isBiggest.validate(3,Seq(2, 3, 4))
        check.isOK should be(false)
      }
    }

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
      it("should be able to check oneOf when all pass") {
        val c = OneOf(Seq(isEven, isPositive))
        val validated = c.validate(2,Seq())
        validated.isOK should be(false)
        val r1: ConstraintReason[Int] = SingleReason(2,"ok even")
        val r2: ConstraintReason[Int] = SingleReason(2,"ok pos")
        validated.errors.size should be(1)
        val e = validated.errors.head
        e.getMessage should be("More than one alternative in oneOf is valid")
      }

      it("should be able to validate oneOf when only one pass") {
        val c = OneOf(Seq(isEven, isPositive))
        val validated = c.validate(3,Seq())
        validated.isOK should be(true)
        val r: ConstraintReason[Int] = SingleReason(3,"ok pos")
        validated.reasons.value.values should contain only (
            Response(r))
      }

      it("should be able to fail validation of oneOf when none pass") {
        val c = OneOf(Seq(isEven, isPositive))
        val validated = c.validate(-3,Seq())
        validated.isOK should be(false)
        validated.errors should contain(NoneValid)
      }

    }

    describe("all") {
      it("should be able to check all when all pass") {
        val c = All(Seq(isEven, isPositive))
        val validated = c.validate(2,Seq())
        validated.isOK should be(true)
        val r1: ConstraintReason[Int] = SingleReason(2,"ok even")
        val r2: ConstraintReason[Int] = SingleReason(2,"ok pos")
        validated.reasons.value.values should contain only (Response(r1), Response(r2))
      }

      it("should be able to fail to validate all when only one pass") {
        val c = All(Seq(isEven, isPositive))
        val validated = c.validate(3,Seq())
        validated.isOK should be(false)
        validated.errors should contain(MsgError("not even"))
      }

      it("should be able to fail validation of all when none pass") {
        val c = All(Seq(isEven, isPositive))
        val validated = c.validate(-3,Seq())
        validated.isOK should be(false)
        validated.errors should contain(MsgError("not even"))
      }

    }
  }
}
