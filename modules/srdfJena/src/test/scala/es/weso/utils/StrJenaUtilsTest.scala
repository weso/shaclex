package es.weso.utils

import org.scalatest._
import StrJenaUtils._

class StrJenaUtilsTest extends FunSpec with Matchers {
  describe("StrUtils unicodeUnscape") {
    shouldUnescape("\\tpepe", "\tpepe")
    shouldUnescape("pepe\\u0031", "pepe1")
    shouldUnescape("\\u0031pepe\\u0031", "1pepe1")
    shouldUnescape("\\u0032\\u00ac\\u0031", "2¬1")
    shouldUnescape("\\\\","\\")
//    shouldUnescape("pe\\-pe", "pe-pe")
//    shouldUnescape("\\U0001D4B8","\uD835\uDCB8")
  }

  def shouldUnescape(str: String, expected: String): Unit = {
    it(s"should unscape $str and obtain $expected") {
      unescape(str) should be(expected)
    }
  }
}