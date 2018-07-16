package es.weso.checking
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest._
import cats._
import data._
import cats.implicits._



class CheckerCatsTest extends FunSpec with Matchers with OptionValues {

  import CheckerCatsStr._

  def runValue_(c:Check[Int]): Either[Err,Int] = runValue(c)(c0)(e0)
  def runLog_(c:Check[Int]): Log = runLog(c)(c0)(e0)
  def runValueFlag(c:Check[(Int,Boolean)]): Either[Err,(Int,Boolean)] = runValue(c)(c0)(e0)

    ignore(s"Checker Cats") {
      it("Should be able to return a value") {
        val c: Check[Int] = ok(2)
        runValue_(c) should ===(Right(2))
      }
      it("Should be able to return an error") {
        val msg = "Error"
        val e: Check[Int] = err(msg)
        runValue_(e) should ===(Left(msg))
      }
      it("Should be able to do an or...") {
        val c: Check[Int] = ok(2)
        val c3: Check[Int] = ok(3)
        val e: Check[Int] = err("Err")
        val e1: Check[Int] = err("Err1")
        runValue_(orElse(c, e)) should ===(Right(2))
        runValue_(orElse(e, c)) should ===(Right(2))
        runValue_(orElse(c, c3)) should ===(Right(2))
        runValue_(orElse(e, e1)) should ===(Left("Err1"))
      }
      it("Should be able to do checkSome...") {
        val c1: Check[Int] = ok(1)
        val c2: Check[Int] = ok(2)
        val c3: Check[Int] = ok(3)
        val e: Check[Int] = err("Err")
        val e1: Check[Int] = err("Err1")
        runValue_(checkSome(List(c1, e), "No one")) should ===(Right(1))
        runValue_(checkSome(List(e, c2, e), "No one")) should ===(Right(2))
        runValue_(checkSome(List(e, e1), "No one")) should ===(Left("No one"))
        runValue_(checkSome(List(c1, c2), "No one")) should ===(Right(1))
      }
      it("Should be able to run local") {
        def addEnv(name: String, value: Int): Env => Env =
          _.updated(name, value)

        val getX: Check[Option[Int]] = for {
          env <- getEnv
        } yield (env.get("x"))
        runValue(getX)(c0)(e0) should ===(Right(None))
        runValue(local(addEnv("x", 1))(getX))(c0)(e0) should ===(Right(Some(1)))

        val c: Check[Option[Int]] = local(addEnv("x", 1))(getX) >> getX
        runValue(c)(c0)(e0) should ===(Right(None))
      }
      it("Should be able to collect a single log") {
        val x1: Check[Int] = for {
          _ <- logStr("L1")
        } yield 1
        val log = runLog_(x1)
        log should ===(List("L1"))
      }
      it("Should be able to collect two logs") {
        val x1: Check[Int] = for {
          _ <- logStr("L1")
        } yield 1
        val x2: Check[Int] = for {
          _ <- logStr("L2")
        } yield 1
        val log = runLog_(x1 >> x2)
        log should ===(List("L1", "L2"))
      }
      it("Should be able to collect two logs with checkSome") {
        val x: Check[Int] = logStr("L1") >> ok(1)
        val e: Check[Int] = logStr("E") >> err("Err")
        runLog_(checkSome(List(x, e), "NoOne")) should ===(List("L1"))
        runLog_(checkSome(List(e, x), "NoOne")) should ===(List("E", "L1"))
        runLog_(checkSome(List(e, e), "NoOne")) should ===(List("E", "E"))
      }
      it("Should be able to execute cond for some successful computation") {
        lazy val x1: Check[Int] = logStr("x1") >> ok(1)
        lazy val x2: Check[Int] = logStr("x2") >> ok(2)
        lazy val e: Check[Int] = logStr("E") >> err("Err")
        lazy val c1 = cond(x1, (_: Int) => x2, _ => e)
        runValue_(c1) should ===(Right(2))
        runLog_(c1) should ===(List("x1", "x2"))
      }
      it("Should be able to execute cond for some fail computation") {
        lazy val x1: Check[Int] = logStr("x1") >> ok(1)
        lazy val x2: Check[Int] = logStr("x2") >> ok(2)
        lazy val e: Check[Int] = logStr("E") >> err("Err")
        lazy val c1 = cond(x1, (_: Int) => e, _ => x2)
        runValue_(c1) should ===(Left("Err"))
        runLog_(c1) should ===(List("x1", "E"))
      }
/*      it("Should be able to update info") {
        def add(x: Int): CheckInfo => CheckInfo = x :: _
        def c : Check[Int] = logStr("X") >> updateInfo(add(1)) >> ok(2)
        runCheck(c)(c0)(e0) should ===(List("X"),(Right(2),List(1)))
      } */
 }

  describe(s"Check some with flag") {
    val counter = new AtomicInteger(0)

    def comp(x: Int): Check[(Int,Boolean)] = {
      counter.getAndIncrement;
      println(s"Comp($x), steps: $counter")
      if (x % 2 == 0) {
        ok((x, true))
      } else {
        ok((x, false))
      }
    }

    counter.set(0)
    shouldCheckSomeFlag("checkSomeFlag(List(comp(2),comp(4)), (0,false)) = (2, true)|1",
      Stream(2, 4),
      comp,
      ok((0,false)),
      (2,true),
      1
    )

    def shouldCheckSomeFlag(msg: String,
                            ls: => Stream[Int],
                            check: Int => Check[(Int,Boolean)],
                            last: => Check[(Int,Boolean)],
                            expected: (Int,Boolean),
                            stepsExpected: Int): Unit = {
      it(msg) {
        runValueFlag(checkSomeFlag(ls, check, last)).fold(e => fail(s"Error: $e"), v => {
          println(s"After checkSome. Value $v, steps: $counter")
          v should be(expected)
          counter.get should equal(stepsExpected)
        })
      }
    }

  }

  ignore(s"Call counter") {

    val counter = new AtomicInteger()

    def f(): Unit = {
      counter.getAndIncrement()
    }

    def cmp: Unit = {
      f();
      f();
    }
      counter.set(0)
      shouldCheck("Cmp(1 with 3 calls)", cmp, (1, true), 2)

    def shouldCheck(msg: String,
                    c: Unit,
                    expected: (Int,Boolean),
                    stepsExpected: Int): Unit = {
      it(msg) {
        c
        counter.get should equal(stepsExpected)
      }
    }

  }

}