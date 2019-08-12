package es.weso.rdf.operations

import es.weso.rdf.nodes._
import org.scalatest._
import cats.implicits._

class ComparisonsTest extends FunSpec with Matchers with TryValues {

  describe("Comparison less than") {

    shouldBeLessThan(DoubleLiteral(2.3), DoubleLiteral(2.4), true)
    shouldBeLessThan(DoubleLiteral(2.3), IntegerLiteral(2), false)
    shouldBeLessThan(DoubleLiteral(2.3), IntegerLiteral(3), true)
    shouldBeLessThan(DoubleLiteral(2.3), DecimalLiteral(2.3), false)
    shouldBeLessThan(DoubleLiteral(2.3), DecimalLiteral(2.34), true)

    def shouldBeLessThan(node1: RDFNode, node2: RDFNode, expected: Boolean): Unit = {
      it(s"lessThan(${node1.show}, ${node2.show}) should be $expected") {
        val r = node1 lessThan node2
        r.fold(e => fail(s"Error: $e"),
          v => v should be(expected)
        )
      }
    }
  }

  describe("Comparison less than or equals") {

    shouldBeLessThan(DoubleLiteral(2.3), DoubleLiteral(2.4), true)
    shouldBeLessThan(DoubleLiteral(2.3), IntegerLiteral(2), false)
    shouldBeLessThan(DoubleLiteral(2.3), IntegerLiteral(3), true)
    shouldBeLessThan(DoubleLiteral(2.3), DecimalLiteral(2.3), true)
    shouldBeLessThan(DoubleLiteral(2.3), DecimalLiteral(2.34), true)

    def shouldBeLessThan(node1: RDFNode, node2: RDFNode, expected: Boolean): Unit = {
      it(s"lessThan(${node1.show}, ${node2.show}) should be $expected") {
        val r = node1 lessThanOrEquals node2
        r.fold(e => fail(s"Error: $e"),
          v => v should be(expected)
        )
      }
    }
  }

  describe("Comparison belongs") {

    shouldContain(List(DoubleLiteral(2.0),IntegerLiteral(2)), IntegerLiteral(2), true)
    shouldContain(List(DoubleLiteral(2.0),IntegerLiteral(2)), IntegerLiteral(3), false)

    def shouldContain(ns: List[RDFNode], node: RDFNode, expected: Boolean): Unit = {
      it(s"contain(${ns.show}, ${node.show}) should be $expected") {
        val r = Comparisons.contains(ns, node)
        r.fold(e => fail(s"Error: $e"),
          v => v should be(expected)
        )
      }
    }
  }

  describe("Comparison not contained") {

    shouldCheckNotContained(List(DoubleLiteral(3.0),IntegerLiteral(2)), List(IntegerLiteral(2)), List(DoubleLiteral(3.0)))
    shouldCheckNotContained(List(DoubleLiteral(2.0),IntegerLiteral(2)), List(IntegerLiteral(2)), List())
    shouldCheckNotContained(List(IntegerLiteral(2)), List(DoubleLiteral(3.0),IntegerLiteral(2)), List())
    shouldCheckNotContained(List(IntegerLiteral(2)), List(IntegerLiteral(3)), List(IntegerLiteral(2)))
    shouldCheckNotContained(List(IntegerLiteral(2)), List(), List(IntegerLiteral(2)))
    shouldCheckNotContained(List(), List(StringLiteral("hi")), List())

    def shouldCheckNotContained(ns: List[RDFNode], ts: List[RDFNode], expected: List[RDFNode]): Unit = {
      it(s"notContained(${ns.show}, ${ts.show}) should be ${expected.show}") {
        val r = Comparisons.notContained(ns, ts)
        r.fold(e => fail(s"Error: $e"),
          vs => vs should contain theSameElementsAs(expected)
        )
      }
    }
  }


  describe("different") {

    shouldCheckDifferent(List(DoubleLiteral(3.0),IntegerLiteral(2)), List(IntegerLiteral(2)), List(DoubleLiteral(3.0)))
    shouldCheckDifferent(List(DoubleLiteral(2.0),IntegerLiteral(2)), List(IntegerLiteral(2)), List())
    shouldCheckDifferent(List(IntegerLiteral(2)), List(DoubleLiteral(3.0),IntegerLiteral(2)), List(DoubleLiteral(3.0)))
    shouldCheckDifferent(List(IntegerLiteral(2)), List(IntegerLiteral(3)), List(IntegerLiteral(2), IntegerLiteral(3)))
    shouldCheckDifferent(List(IntegerLiteral(2)), List(), List(IntegerLiteral(2)))
    shouldCheckDifferent(List(), List(StringLiteral("hi")), List(StringLiteral("hi")))

    def shouldCheckDifferent(ns: List[RDFNode], ts: List[RDFNode], expected: List[RDFNode]): Unit = {
      it(s"different(${ns.show}, ${ts.show}) should be ${expected.show}") {
        val r = Comparisons.different(ns, ts)
        r.fold(e => fail(s"Error: $e"),
          vs => vs should contain theSameElementsAs(expected)
        )
      }
    }
  }
}