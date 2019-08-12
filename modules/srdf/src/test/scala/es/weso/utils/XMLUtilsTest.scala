package es.weso.utils

import es.weso.utils.XMLUtils._
import org.scalatest._

class XMLUtilsTest extends FunSpec with Matchers {

  describe("lessThanXMLDateTime") {
    shouldLessThanXMLDatetyme("2012", "2013", Right(true))
    shouldLessThanXMLDatetyme("2013", "2012", Right(false))
    shouldLessThanXMLDatetyme("2013-10-10", "2013-10-11", Right(true))
    shouldLessThanXMLDatetyme("2013-10-10T12:00:00", "2013-10-11", Right(true))

    // I'm not sure about the following two tests. They seem to work but oddly, the result is always false even
    // if the dates are reversed. Anyway, the behaviour seems to follow the Java compare function:
    // https://docs.oracle.com/javase/7/docs/api/javax/xml/datatype/XMLGregorianCalendar.html
    shouldLessThanXMLDatetyme("2002-10-10T12:00:01-05:00", "2002-10-10T12:00:00", Right(false))
    shouldLessThanXMLDatetyme("2002-10-10T12:00:00", "2002-10-10T12:00:00-05:00", Right(false))

    shouldLessThanXMLDatetyme("2013-10-10T00:00:00", "2013-10-10", Right(false)) // are equal
    shouldLessThanXMLDatetyme("???-10-10T00:00:00", "2013-10-10", Left("error parsing"))
    shouldLessThanXMLDatetyme("2014-10-10T00:00:00", "???-10-10", Left("error parsing"))



    def shouldLessThanXMLDatetyme(s1: String, s2: String, expected: Either[String, Boolean]): Unit = {
      it(s"Should calculate lessThanXSDDateTime($s1,$s2) and return $expected") {
        lessThanXSDDateTimes(s1, s2).fold(e =>
          expected.fold(
            es => info(s"Failed with message $e as expected"),
            eb => fail(s"Failed with message $e but should have returned $expected")
          ), v =>
          expected.fold(
            es => fail(s"Returned $v but should have failed with $es"),
            eb => v should be(eb)
          )
        )
      }
    }
  }
}