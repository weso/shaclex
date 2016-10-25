package es.weso.utils
import SeqUtils._
import org.scalatest._

class SeqUtilsTest extends FunSpec with Matchers {
  describe("Transpose") {
    it ("Should check three elements") {
      val xs: List[(Char, List[Int])] =
        List(('a', List(1, 2)),
             ('b', List(0)),
             ('c', List(3, 4)))
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
    val xs: List[(Char, List[Int])] =
      List(('a', List()),
           ('b', List()),
           ('c', List()))
    val expected: List[List[(Char, Int)]] =
      List()
    transpose(xs) should be(expected)
  }

}