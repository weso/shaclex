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

  describe(s"getPrefixLocalName") {

    testGetPrefixLocalName(IRI("http://example.org/foo"),
      PrefixMap(Map(
      Prefix("a") -> IRI("http://example.org/"),
      Prefix("r") -> IRI(s"http://example.org/reference/")
    )), Prefix("a"), "foo"
    )

    testGetPrefixLocalName(IRI("http://example.org/reference/foo"),
      PrefixMap(Map(
        Prefix("a") -> IRI("http://example.org/"),
        Prefix("r") -> IRI(s"http://example.org/reference/")
      )), Prefix("r"), "foo"
    )


    def testGetPrefixLocalName(iri: IRI, pm: PrefixMap, expectedPrefix: Prefix, expectedLocalName: String): Unit = {
      it(s"Should getPrefixLocalName($iri) and obtain ($expectedPrefix, $expectedLocalName)") {
        pm.getPrefixLocalName(iri).fold(e => fail(s"Error $e"), values => {
          val (prefix, iri, str) = values
          prefix should be(expectedPrefix)
          str should be(expectedLocalName)
         }
        )
      }
    }
  }
}