package es.weso.rdf.dot

import es.weso.rdf.jena.RDFAsJenaModel
import org.scalatest.{FunSpec, Matchers}

class DotTest extends FunSpec with Matchers {

  describe("Dot") {
    it("Should generate from empty RDF") {
      RDFAsJenaModel.fromChars(
        """|prefix : <http://example.org/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |:x :a 1, :y .
           |:y :b "Hi" .
           |:z :c "Hola"@es .
           |:z :c "1984"^^<xsd:year> .
           |:z :a :x .
        """.stripMargin, "TURTLE", None).fold(e => fail(s"Error: $e"),
        rdf => {
          val dot = RDF2Dot.rdf2dot(rdf)
          println(s"Size of triples: ${rdf.rdfTriples().size}")
          println(s"Dot generated: $dot")
          dot.edges.size should be(3)
        }
      )
    }
  }
}