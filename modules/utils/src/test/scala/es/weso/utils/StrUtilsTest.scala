package es.weso.utils

import org.scalatest._
import StrUtils._

class StrUtilsTest extends FunSpec with Matchers {

  describe("StrUtils unescape") {
    shouldUnescape("\\tpepe", "\tpepe")
    shouldUnescape("pepe\\u0031", "pepe1")
    shouldUnescape("\\u0031pepe\\u0031", "1pepe1")
    shouldUnescape("\\u0032\\u00ac\\u0031", "2¬1")
    shouldUnescape("pe\\-pe", "pe-pe")
    shouldUnescape("\\\\","\\")
    shouldUnescape("\\U0001D4B8","\uD835\uDCB8")
  }

  def shouldUnescape(str: String, expected: String): Unit = {
    it(s"should unescape $str and obtain $expected") {
      unescape(str) should be(expected)
    }
  }

  describe("StrUtils escape") {
    shouldEscape("\tpepe", "\\tpepe")
    shouldEscape("pepe1", "pepe1")
    shouldEscape("1pepe1", "1pepe1")
    shouldEscape("2\u00ac1", "2\u00ac1")
    shouldEscape("pe-pe", "pe\\-pe")
    shouldEscape("\\","\\\\")
    // shouldUnescape("\\U0001D4B8","\uD835\uDCB8")
  }

  def shouldEscape(str: String, expected: String): Unit = {
    it(s"should escape $str and obtain $expected") {
      escape(str) should be(expected)
    }
  }
}