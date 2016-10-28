package es.weso.utils
import SeqUtils._
import org.scalatest._

class SeqUtilsTest extends FunSpec with Matchers {
  describe("Transpose") {
    it ("Should check three elements") {
      val xs: List[(Char, Set[Int])] =
        List(('a', Set(1, 2)),
             ('b', Set(0)),
             ('c', Set(3, 4)))
      val expected: List[List[(Char, Int)]] =
        List(
          List(('a', 1), ('b', 0), ('c', 3)),
          List(('a', 1), ('b', 0), ('c', 4)),
          List(('a', 2), ('b', 0), ('c', 3)),
          List(('a', 2), ('b', 0), ('c', 4))
        )
      transpose(xs) should be(expected)
    }
  }
  it ("Should check three elements that fail") {
    val xs: List[(Char, Set[Int])] =
      List(('a', Set[Int]()),
           ('b', Set[Int]()),
           ('c', Set[Int]()))
    val expected: List[List[(Char, Int)]] =
      List()
    transpose(xs) should be(expected)
  }
  describe("Intersperse") {
    it ("should intersperse 1,2,3 with comma") {
      val xs = List("1", "2", "3")
      val c = ","
      val expected = List("1",",","2",",","3")
      intersperse(c, xs) should be(expected)
    }
    it ("should intersperse empty list with comma") {
      val xs = List[String]()
      val c = ","
      val expected = List[String]()
      intersperse(c, xs) should be(expected)
    }
    it ("should intersperse singleton list with comma") {
      val xs = List("1")
      val c = ","
      val expected = List("1")
      intersperse(c, xs) should be(expected)
    }
  }
}