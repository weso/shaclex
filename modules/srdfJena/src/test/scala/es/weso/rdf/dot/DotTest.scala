package es.weso.rdf.dot

import es.weso.rdf.jena.RDFAsJenaModel
import org.scalatest.{FunSpec, Matchers}

class DotTest extends FunSpec with Matchers {

  describe("Dot") {
    it("Should generate from example") {
      RDFAsJenaModel.fromChars(
        """|prefix : <http://example.org/>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |
           |:x :a 1, :y, _:1 .
           |:x :b ("a" "b") .
           |:y :b "Hi" .
           |:z :c "Hola"@es .
           |:z :c "1984"^^<xsd:year> .
           |_:1 :a :z .
           |:z :a :x .
        """.stripMargin, "TURTLE", None).fold(e => fail(s"Error: $e"),
        rdf => {
          val dot = RDF2Dot.rdf2dot(rdf)
          println(s"Size of triples: ${rdf.rdfTriples().size}")
          println(s"Dot generated: $dot")
          dot.edges.size should be(13)
        }
      )
    }

    it("Should generate from RDF with 2 overlapping prefixes") {
      RDFAsJenaModel.fromChars(
        """|prefix e: <http://example.org/>
           |prefix ep: <http://example.org/p/>
           |e:x ep:a ep:y .
        """.stripMargin, "TURTLE", None).fold(e => fail(s"Error: $e"),
        rdf => {
          val dot = RDF2Dot.rdf2dot(rdf)
          println(s"Size of triples: ${rdf.rdfTriples().size}")
          println(s"Dot generated: $dot")
          dot.edges.size should be(1)
        }
      )
    }

  }

  it("Should generate from RDF with literal") {
    RDFAsJenaModel.fromChars(
      """|<x> <p> <y> ; <p> "Hi" .
      """.stripMargin, "TURTLE", None).fold(e => fail(s"Error: $e"),
      rdf => {
        val dot = RDF2Dot.rdf2dot(rdf)
        println(s"Size of triples: ${rdf.rdfTriples().size}")
        println(s"Dot generated: $dot")
        dot.edges.size should be(2)
      }
    )
  }

}