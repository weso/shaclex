package es.weso.rdf

import org.scalatest._
import es.weso.rdf.nodes._

class PrefixesTest extends FunSpec with Matchers with TryValues {

  describe("PrefixesTest") {
    it("can get xsd prefix") {
      val xsd = PREFIXES.xsd
      val iri_xsd = IRI("http://www.w3.org/2001/XMLSchema#")
      iri_xsd.str should be(xsd.str)
    }
  }
}