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
        val r = Comparisons.lessThan(node1, node2)
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
        val r = Comparisons.lessThanOrEquals(node1, node2)
        r.fold(e => fail(s"Error: $e"),
          v => v should be(expected)
        )
      }
    }
  }

}