package es.weso.converter

import org.scalatest._
import scala.util.Try

class ConverterTest extends FunSpec with Matchers with Converter {

  describe(s"Converter") {
    it(s"Should convert strings to ints") {
      def cnvStr(str: String): Result[Int] = {
        Try(Integer.parseInt(str)).fold(exc => err(exc.getMessage), ok(_))
      }

      def cnvList(ls: List[String]): Result[List[Int]] = {
        sequence(ls.map(cnvStr(_)))
      }

      def checkPositive(n: Int): Result[Int] = {
        if (n >=0) ok(n)
        else err(s"Negative value ${n}")
      }

      def cnvListPositive(ls: List[String]): Result[List[Int]] = {
        val xs: Result[List[Int]] = cnvList(ls)
        def next(ls: List[Int]): Result[List[Int]] = sequence(ls.map(n => checkPositive(n)))
        xs andThen(next)
      }

      shouldBeEqualTo(cnvStr("23"),23)
      shouldBeEqualTo(cnvStr("0"),0)
      shouldBeEqualTo(cnvList(List("0","1")),List(0,1))
      shouldBeEqualTo(cnvListPositive(List("2","3")),List(2,3))
      shouldFail(cnvListPositive(List("2","-1")))

    }
  }

  def shouldBeEqualTo[A](r: Result[A], expected: A): Unit = {
    r.fold(ls => {
      fail(s"Errors: ${ls.toList.mkString(",")}")
    }, n => n should be(expected)
    )
  }

  def shouldFail[A](r: Result[A]): Unit = {
    r.isValid should be(false)
  }
}