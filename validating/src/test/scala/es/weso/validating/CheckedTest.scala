package es.weso.validating
import CheckedSub._
import NDResponse._
import org.scalatest._
import cats.implicits._

class CheckedTest
 extends FunSpec with Matchers with OptionValues {

  type Explain[A] = Option[A]
  type E = Throwable
  type CheckedSingle = Checked[Int,Explain[Int],E]
  type CheckedSeq = Checked[Seq[Int],Explain[Int],E]

  describe("Checked") {

    it("Checks simple ok") {
      val x2: Checked[Int,List[Int],Throwable] = ok(Some(2),List(2))
      x2.isOK should be(true)
    }

    it("Checks simple error") {
      val e: Checked[Int,List[Int],Throwable] = err(Some(2),new Exception("Error"))
      e.isOK should be(false)
    }

    it("Converts an ok value into an error") {
      val v: Checked[Int,Int,Throwable] = ok(Some(2),2)
      val hi = new Exception("hi")
      val e = v.addError(hi)
      e.isOK should be(false)
      e.errors should contain only(hi)
    }

    it("Accumulates several errors") {
      val v: Checked[Int,Int,Throwable] =
        ok(Some(2),2)
      val hi  = new Exception("hi")
      val man = new Exception("hi")
      val e = v.addErrors(Seq(hi,man))
      e.isOK should be(false)
      e.errors should contain only(hi,man)
    }

    it("should be able to fold an ok value") {
      val v: Checked[Int,Int,Throwable] = ok(Some(2),2)
      val folded = v.fold((x,rs) => x.map(_ + 1), (_,_) => 0)
      folded should be(Some(3))
    }

    it("should be able to fold an errored value") {
      val v: Checked[Int,Int,Throwable] = err(Some(2),new Exception("Hi"))
      val folded = v.fold((x,rs) => x.map(_ + 1), (_,_) => 0)
      folded should be(0)
    }

    it("should be able to merge a value with a list") {
      val v1: Checked[Int,String,Throwable] = ok(Some(1),"1")
      val v23: Checked[Seq[Int],String,Throwable] = ok(Some(Seq(2,3)),"23")
      val v = merge(v1,v23)
      v.isOK should be(true)
      val expected: Seq[String] = Seq("123")
      v.value should be(Some(Seq(1,2,3)))
      v.responses should be(expected)
    }
  }

    describe("all") {
      it("should be able to pass when one pass") {
        val v1: CheckedSingle = oks(Some(2),Seq(Some(1),Some(2)))
        val v2: CheckedSingle = oks(Some(3),Seq(Some(2),Some(3)))
        val vs : Seq[CheckedSingle] = Seq(v1,v2)
        val vall: CheckedSeq = checkAll(vs)
        vall.isOK should be(true)
        vall.errors should be(Seq())
        val reasons = vall.responses
        val expected : Seq[Option[Int]] = Seq(Some(3),Some(4),Some(4),Some(5))
        reasons should be(expected)
      }

      it("should fail when one fails") {
        val v1: CheckedSingle = err(Some(1),new Exception("err"))
        val v2: CheckedSingle = ok(Some(2),Some(2))
        val vs : Seq[CheckedSingle] = Seq(v1,v2)
        val vall: CheckedSeq = checkAll(vs)
        vall.isOK should be(false)
      }
    }

  describe("Checked custom errors") {
    case class MyError[A](msg:String) extends ConstraintError[A]
    type CheckedCustom = Checked[Int,Explain[Int],MyError[Int]]

    it("can raise a custom error") {
      val e: MyError[Int] = MyError("Hi")
      val v1 : Checked[Int,Explain[Int],MyError[Int]] = err(Some(1),e)
      v1.isOK should be(false)
    }
    it("can raise a custom error or return ok") {
      val e: MyError[Int] = MyError("Hi")
      val v1 : Checked[Int,Explain[Int],MyError[Int]] =
        if (4 % 2 == 1) err(Some(4),e)
        else ok(Some(2),Some(2))
      v1.isOK should be(true)
    }
   it("can calculate all with custom errors") {
      val e: MyError[Int] = MyError("Hi")
      val v1 : Checked[Int,Explain[Int],MyError[Int]] = ok(Some(1),Some(1))
      val v2 : Checked[Int,Explain[Int],MyError[Int]] = ok(Some(2),Some(2))
      val vs : Checked[Seq[Int],Explain[Int],MyError[Int]] = checkAll(Seq(v1,v2))
      vs.isOK should be(true)
    }
  }
}
