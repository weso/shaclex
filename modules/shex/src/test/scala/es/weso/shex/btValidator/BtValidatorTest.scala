package es.weso.shex.btValidator

import org.scalatest._
import BtValidator._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex.Schema

class BtValidatorTest extends FunSpec with Matchers with EitherValues {

  describe(s"BtValidator semantics") {
    it(s"Should validate OK") {
      val c: Check[Int] = ok(3)
      val rdf = RDFAsJenaModel.empty
      val schema = Schema.empty
      val r = runCheck(rdf, schema, c)
      r.fold(e => fail(s"Error: $e"),
        n => n should be(3))
    }
  }
}
