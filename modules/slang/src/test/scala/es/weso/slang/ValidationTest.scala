package es.weso.slang
import org.scalatest.matchers.should.Matchers 
import org.scalatest.funspec.AnyFunSpec

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shex.Schema
// import cats.data._ 
import cats.effect._ 
import es.weso.utils.IOUtils._

class ValidationTest extends AnyFunSpec with Matchers with SLang2Clingo with ShEx2SLang {

  describe(s"SLang validation") {
    it(s"Should validate simple example") {
      val node = IRI("http://example.org/a")
      val shape: SLang  = Ref(IRILabel(IRI("http://example.org/User")))
      val r: IO[ShapesMap] = RDFAsJenaModel.fromChars(
          """|<a> <name> "a" ;
             | <knows> <a> .
            |
          """.stripMargin, "TURTLE",Some(IRI("http://example.org/"))).flatMap(_.use(rdf => for {
        schema <- Schema.fromString(
          """|
             |<User> {
             | <name> . ;
             | <knows> .
             |}
          """.stripMargin, "ShEXC",Some(IRI("http://example.org/")))
        slangSchema <- shex2SLang(schema)
        eitherResult <- Validation.runValidation(node, shape, rdf, slangSchema)
        result <- fromES(eitherResult)
      } yield result))

      r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), result => {
        result.isConforming(node, shape) should be(Conforms)
      })
    }
  }
}
