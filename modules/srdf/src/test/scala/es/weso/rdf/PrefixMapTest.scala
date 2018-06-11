package es.weso.rdf

import es.weso.rdf.nodes._
import org.scalatest._

class PrefixMapTest extends FunSpec with Matchers with TryValues {

  describe("qualify") {
      val pm = PrefixMap.empty.
        addPrefix("e",IRI("http://example.org/")).
        addPrefix("ep",IRI("http://example.org/p/"))


      qualifyTest(IRI("http://example.org/x"),pm,"e:x")
      qualifyTest(IRI("http://other.org/x"), pm, "<http://other.org/x>")
      qualifyTest(IRI("http://example.org/p/x"), pm, "ep:x")
    }

   def qualifyTest(iri:IRI, pm:PrefixMap, expected: String): Unit = {
     it(s"should qualify $iri to obtain $expected") {
       pm.qualify(iri) should be(expected)
     }
  }
}