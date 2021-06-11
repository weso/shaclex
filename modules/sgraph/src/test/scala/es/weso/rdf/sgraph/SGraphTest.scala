package es.weso.rdf.sgraph

import es.weso.rdf.jena.RDFAsJenaModel

// import es.weso.utils.test._
// import org.scalatest.matchers.should._ 
// import org.scalatest.funspec.AnyFunSpec
// import cats.data._
// import cats.effect._
// import es.weso.utils.IOUtils._
import munit._
import cats.effect.IO
import io.circe._
import io.circe.parser._
import cats.implicits._

class SGraphTest extends CatsEffectSuite {

 test("Should generate from example") {
      val e = RDFAsJenaModel.fromChars(
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
            """.stripMargin,
            "TURTLE",
            None
          ).flatMap(_.use(rdf => for {
       ts <- rdf.rdfTriples.compile.toList
       dot <-RDF2SGraph.rdf2sgraph(rdf)
      } yield (rdf,ts,dot)))
      e.attempt.unsafeRunSync.fold(
          e => fail(s"Error: $e"),
        tuple => {
            val (_,ts,dot) = tuple
            println(s"RDF parsed: ${ts.size}")
            println(s"Size of triples: ${ts.size}")
            println(s"Dot generated: $dot")
            assertEquals(dot.edges.size, 13)
          }
        )
    }


    /*    it("Should generate from RDF with 2 overlapping prefixes") {
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
*/

  test(s"Should convert to Json") {
      val expectedStr =
        """|[
           |{ "data": { "id": "N0", "label": ":x", "type": "iri" } },
           |{ "data": { "id": "N1", "label": ":y", "type": "iri" } },
           |{ "data": { "id": "N2", "label": ":A", "type": "iri" } },
           |{ "data": { "source": "N0", "target": "N2", "label": "a", "href": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" } },
           |{ "data": { "source": "N0", "target": "N1", "label": ":p", "href": "http://example.org/p" } }
           |]
           |""".stripMargin

      val eitherParsed: Either[Throwable,Json] = parse(expectedStr).leftMap(e => new RuntimeException(e))

      val cmp: IO[(Json,Json)] = RDFAsJenaModel.fromString(
          """prefix : <http://example.org/>
            |:x a :A ;
            |   :p :y .
            |""".stripMargin, "TURTLE"
        ).flatMap(_.use(rdf => for {
        sg <- RDF2SGraph.rdf2sgraph(rdf)
        expected <- IO.fromEither(eitherParsed)
      } yield (sg.toJson, expected)))
      cmp.map { case (json,expected) => assertEquals(json, expected) }
    }
  }
