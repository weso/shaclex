package es.weso.slang
import org.scalatest._

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shex.Schema


class ValidationTest extends FunSpec
  with Matchers with SLang2Clingo with ShEx2SLang with EitherValues {

  describe(s"SLang validation") {
    it(s"Should validate simple example") {
      val r = for {
        rdf <- RDFAsJenaModel.fromChars(
          """|<a> <name> "a" ;
             | <knows> <a> .
            |
          """.stripMargin, "TURTLE",Some(IRI("http://example.org/")))
        schema <- Schema.fromString(
          """|
             |<User> {
             | <name> . ;
             | <knows> .
             |}
          """.stripMargin, "ShEXC",Some(IRI("http://example.org/")))
        slangSchema <- shex2SLang(schema)
      } yield (rdf,schema,slangSchema)

      r.fold(e => fail(s"Error: $e"), values => {
        val (rdf,schema,slangSchema) = values
        val node = IRI("http://example.org/a")
        val shape: SLang  = Ref(IRILabel(IRI("http://example.org/User")))
        val result = Validation.runValidation(node, shape, rdf, slangSchema).right.value
        info(s"SLang schema: $slangSchema")
        info(s"Result: ${result.map(node)}")
        result.isConforming(node, shape) should be(Conforms)
      })
    }
  }
}
