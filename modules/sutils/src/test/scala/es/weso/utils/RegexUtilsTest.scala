package es.weso.utils

import org.scalatest._

class RegexUtilsTest extends FunSpec with Matchers {
  describe("Regex") {
    shouldMatch("\\d{2}", None, "34")
    // shouldMatch("""^\\/\t\\n\\r$""", None, "/\t\n\r")
  }


  def shouldMatch(regex: String, flags: Option[String], str: String): Unit = {
    it(s"should match /$regex/${flags.getOrElse("")} with $str") {
      RegEx(regex, flags).matches(str) match {
        case Right(true) => info(s"$str matches /$regex/${flags.getOrElse("")}")
        case Right(false) => fail(s"$str doesn't match /$regex/${flags.getOrElse("")}")
        case Left(msg) => fail(s"Error $msg trying to match $str with /$regex/${flags.getOrElse("")}")
      }
    }
  }
}