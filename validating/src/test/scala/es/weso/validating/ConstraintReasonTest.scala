package es.weso.validating
import Constraint._
import org.scalatest._
import cats.implicits._

class ConstraintReasonTest extends FunSpec with Matchers with OptionValues {

  describe("ConstraintReason") {

/*    it("Can canculate product of two single constraint reasons") {
      val cr1: ConstraintReason[Int] = SingleReason("one")
      val cr2: ConstraintReason[Int] = SingleReason("two")
      val cr = cr1 product cr2
      cr should be(SingleReason((1, 2), "onetwo"))
    }

    ignore("Can calculate product of some with all ") {
      val ls1 = Seq(SingleReason(1, "one"), SingleReason(2, "two"))
      val ls2 = Seq(SingleReason(3, "three"), SingleReason(4, "four"))
      val cr1: ConstraintReason[Int] = AllReason(ls1)
      val cr2: ConstraintReason[Int] = SomeReason(ls2)
      val cr = cr1 product cr2
      cr should be(1)
    }

    ignore("Can calculate product of all with some") {
      val ls1 = Seq(SingleReason(1, "one"), SingleReason(2, "two"))
      val ls2 = Seq(SingleReason(3, "three"), SingleReason(4, "four"))
      val cr1: ConstraintReason[Int] = AllReason(ls1)
      val cr2: ConstraintReason[Int] = SomeReason(ls2)
      val cr = cr2 product cr1
      cr should be(1)
    } */
  }

}
