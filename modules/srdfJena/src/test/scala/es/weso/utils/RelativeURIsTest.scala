package es.weso.utils

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.rdf.triples.RDFTriple
import org.scalatest._

class RelativeURIsTest extends FunSpec with Matchers {
  describe("Relative URIs") {
    it(s"Should parse Turtle with relative URIs") {
      val str = """<x> <p> <y>"""
      val base = Some(IRI("internal://base/"))
      val r = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", base)
        x <- IRI.fromString("x", base)
        p <- IRI.fromString("p",base)
        y <- IRI.fromString("y",base)
        ts <- rdf.triplesWithSubject(x)
        serialized <- rdf.serialize("TURTLE",base)
      } yield {
        (ts,rdf,serialized,x,p,y)
      }
      r.fold(e => fail(s"Error: $e"),
        pair => {
          val (ts,rdf,str,x,p,y) = pair
          println(str)
          ts should contain theSameElementsAs(Set(RDFTriple(x,p,y)))
        }
      )
    }
  }

}