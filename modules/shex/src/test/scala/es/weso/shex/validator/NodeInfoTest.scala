package es.weso.shex.validator

import es.weso.rdf.jena._
import es.weso.rdf.nodes._
import es.weso.shex._
import org.scalatest._
import es.weso.rdf.PREFIXES._

class NodeInfoTest extends FunSpec with Matchers with EitherValues {

  describe("totalDigits") {
    it("Should calculate total digits of 3.14") {
      val d = DecimalLiteral(3.14)
      NodeInfo.totalDigits(d) should be(3)
    }
    it("Should calculate total digits of 3.14 as datatype literal") {
      val d = DatatypeLiteral("3.14",xsd_decimal)
      NodeInfo.totalDigits(d) should be(3)
    }
    it("Should calculate total digits of 3.123456 as datatype literal") {
      val d = DatatypeLiteral("3.123456",xsd_decimal)
      NodeInfo.totalDigits(d) should be(7)
    }
    it("Should calculate total digits of true and return 0") {
      val d = BooleanLiteral(true)
      NodeInfo.totalDigits(d) should be(0)
    }
  }
  describe("fractionDigits") {
    it("Should calculate fraction digits of 3.14") {
      val d = DecimalLiteral(3.14)
      NodeInfo.fractionDigits(d) should be(2)
    }
    it("Should calculate fraction digits of 3.14 as datatype literal") {
      val d = DatatypeLiteral("3.14",xsd_decimal)
      NodeInfo.fractionDigits(d) should be(2)
    }
    it("Should calculate fraction digits of 3.123456 as datatype literal") {
      val d = DatatypeLiteral("3.123456",xsd_decimal)
      NodeInfo.fractionDigits(d) should be(6)
    }
    it("Should calculate fraction digits of true and return 0") {
      val d = BooleanLiteral(true)
      NodeInfo.fractionDigits(d) should be(0)
    }
  }
}
