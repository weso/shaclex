package es.weso.shex.validator

import org.scalatest._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shex.Schema

class IRITest extends FunSpec with Matchers with EitherValues {
  val rdf = RDFAsJenaModel.empty

  describe(s"Test IRI") {
    it(s"Resolves basic") {
      val r = for {
       rdf <- RDFAsJenaModel.fromChars("""|<x> <p> 1""".stripMargin, "TURTLE", Some(IRI("http://example.org/")))
       schema <- Schema.fromString("""|<S> { <p> . }""".stripMargin, "ShExC", Some(IRI("http://example.org/")))
      } yield rdf
      r.fold(e => fail(s"Error $e"), values => {
        val rdf = values
        rdf.triplesWithSubject(IRI("http://example.org/x")).size should be(1)
      })
    }
  }
}
