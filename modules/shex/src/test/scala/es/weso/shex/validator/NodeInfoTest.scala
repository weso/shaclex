package es.weso.shex.validator

import es.weso.rdf.nodes._
import org.scalatest._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.jena.RDFAsJenaModel

class NodeInfoTest extends FunSpec with Matchers with EitherValues {
  val rdf = RDFAsJenaModel.empty

  describe("totalDigits") {
    it("Should calculate total digits of 3.14") {
      val d = DecimalLiteral(3.14)
      NodeInfo.totalDigits(d,rdf).fold(e => fail(s"Error: $e"), n => n should be(3))
    }
    it("Should calculate total digits of 3.14 as datatype literal") {
      val d = DatatypeLiteral("3.14",xsd_decimal)
      NodeInfo.totalDigits(d,rdf).fold(e => fail(s"Error: $e"), n => n should be(3))
    }
    it("Should calculate total digits of 3.123456 as datatype literal") {
      val d = DatatypeLiteral("3.123456",xsd_decimal)
      NodeInfo.totalDigits(d,rdf).fold(e => fail(s"Error: $e"), n => n should be(7))
    }
    it("Should calculate total digits of true and return 0") {
      val d = BooleanLiteral(true)
      NodeInfo.totalDigits(d,rdf).fold(e => fail(s"Error: $e"), n => n should be(0))
    }
  }
  describe("fractionDigits") {
    it("Should calculate fraction digits of 3.14") {
      val d = DecimalLiteral(3.14)
      NodeInfo.fractionDigits(d,rdf).fold(e => fail(s"Error: $e"), n => n should be(2))
    }
    it("Should calculate fraction digits of 3.14 as datatype literal") {
      val d = DatatypeLiteral("3.14",xsd_decimal)
      NodeInfo.fractionDigits(d,rdf).fold(e => fail(s"Error: $e"), n => n should be(2))
    }
    it("Should calculate fraction digits of 3.123456 as datatype literal") {
      val d = DatatypeLiteral("3.123456",xsd_decimal)
      NodeInfo.fractionDigits(d,rdf).fold(e => fail(s"Error: $e"), n => n should be(6))
    }
    it("Should calculate fraction digits of true and return 0") {
      val d = BooleanLiteral(true)
      NodeInfo.fractionDigits(d,rdf).fold(e => fail(s"Error: $e"), n => n should be(0))
    }
  }
}
