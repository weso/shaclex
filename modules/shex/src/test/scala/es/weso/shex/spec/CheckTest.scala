package es.weso.shex.spec

import Check._
import es.weso.shex.Schema
import org.scalatest._

class CheckTest extends FunSpec with Matchers with EitherValues {
/*
  describe(s"satisfy all") {

    it(s"Should satisfy a list of values when all true") {
      val opt1 = pure(true)
      val opt2 = pure(true)
      runCheck(Schema.empty, satisfyAll(List(opt1, opt2))).fold(
        e => fail(s"Error"),
        v => v should be(true)
      )
    }

    it(s"Should not satisfy a list of values when one is false") {
      val opt1 = pure(true)
      val opt2 = pure(false)
      runCheck(Schema.empty, satisfyAll(List(opt1, opt2))).fold(
        e => fail(s"Error"),
        v => v should be(false)
      )
    }
    it(s"Should fail when one fails") {
      val opt1: Check[Boolean] = pure(true)
      val opt2: Check[Boolean] = err(s"Error")
      runCheck(Schema.empty, satisfyAll(List(opt1, opt2))).fold(
        e => info(s"Fails as expected $e"),
        v => fail(s"Should fail but returned $v")
      )
    }
  }

  describe(s"optSatisfy") {
    def check(x: Int): Check[Boolean] =
      if (x == 0) pure(true)
      else pure(false)

    it(s"Should satisfy a none") {
      runCheck(Schema.empty, optSatisfy(None, check)).
        fold(e => fail(s"Failed with none but it should be true"),
          v => v should be(true)
        )
    }
    it(s"Should satisfy an ok value (0)") {
      runCheck(Schema.empty, optSatisfy(Some(0), check)).
        fold(e => fail(s"Failed with none but it should be true"),
          v => v should be(true)
        )
    }
    it(s"Should not satisfy an incorrect value (non 0") {
      runCheck(Schema.empty, optSatisfy(Some(1), check)).
        fold(e => fail(s"Failed with none but it should be true"),
          v => v should be(false)
        )
    }
  }

  describe(s"satisfySome") {

    it(s"Should satisfy a list of values when all true") {
      val opt1: Check[Boolean] = pure(true)
      val opt2: Check[Boolean] = pure(true)
      runCheck(Schema.empty, satisfySome(List(opt1, opt2))).fold(
        e => fail(s"Error"),
        v => v should be(true)
      )
    }

    it(s"Should satisfy a list of values when one is false but another is true") {
      val opt1: Check[Boolean] = pure(false)
      val opt2: Check[Boolean] = pure(true)
      runCheck(Schema.empty, satisfySome(List(opt1, opt2))).fold(
        e => fail(s"Error"),
        v => v should be(true)
      )
    }

    it(s"Should fail when one fails") {
      val opt1: Check[Boolean] = pure(true)
      val opt2: Check[Boolean] = err(s"Error")
      runCheck(Schema.empty, satisfySome(List(opt1, opt2))).fold(
        e => info(s"Fails as expected $e"),
        v => fail(s"Should fail but returned $v")
      )
    }
    it(s"Should fail when all fail") {
      val opt1: Check[Boolean] = pure(false)
      val opt2: Check[Boolean] = pure(false)
      runCheck(Schema.empty, satisfySome(List(opt1, opt2))).fold(
        e => fail(s"Error $e"),
        v => v should be(false)
      )
    }
  }

  describe(s"satisfyNot") {

    it(s"Should satisfy a failing check") {
      val opt: Check[Boolean] = pure(true)
      runCheck(Schema.empty, satisfyNot(opt)).fold(
        e => fail(s"Error"),
        v => v should be(false)
      )
    }

    it(s"Should not satisfy a failing check") {
      val opt: Check[Boolean] = pure(false)
      runCheck(Schema.empty, satisfyNot(opt)).fold(
        e => fail(s"Error"),
        v => v should be(true)
      )
    }
    it(s"Should err an error check") {
      val opt: Check[Boolean] = err("Error")
      runCheck(Schema.empty, satisfyNot(opt)).fold(
        e => info(s"Error as expected"),
        v => fail(s"Should return error and returned $v")
      )
    }

  }
*/
  describe(s"satisfyFirst") {

    it(s"Should satisfyFirst on 1,2,3") {
      val ls = Stream(1,2,3)
      def even(n: Int): Check[Boolean] = {
        if (n < 0) err(s"Negative")
        else pure(n % 2 == 0)
      }
      runCheck(Schema.empty, satisfyFirst(ls, even)).fold(
        e => fail(s"Error"),
        v => v should be(true)
      )
    }

    it(s"Should not satisfyFirst on 1,3,5") {
      val ls = Stream(1,3,5)
      def even(n: Int): Check[Boolean] = {
        if (n < 0) err(s"Negative")
        else pure(n % 2 == 0)
      }
      runCheck(Schema.empty, satisfyFirst(ls, even)).fold(
        e => fail(s"Error"),
        v => v should be(false)
      )
    }

    it(s"Should satisfyFirst on infinite") {
      val ls = Stream.from(1).take(5)
      def even(n: Int): Check[Boolean] = {
        if (n < 0) err(s"Negative")
        else pure(n % 2 == 0)
      }
      runCheck(Schema.empty, satisfyFirst(ls, even)).fold(
        e => fail(s"Error"),
        v => v should be(true)
      )
    }

  }
}
