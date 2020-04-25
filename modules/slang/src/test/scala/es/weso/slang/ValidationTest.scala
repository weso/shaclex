package es.weso.slang
import org.scalatest.matchers.should.Matchers 
import org.scalatest.funspec.AnyFunSpec

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shex.Schema
import cats.data._ 
import cats.effect._ 
import es.weso.utils.IOUtils._

class ValidationTest extends AnyFunSpec with Matchers with SLang2Clingo with ShEx2SLang {

  describe(s"SLang validation") {
    it(s"Should validate simple example") {
      val node = IRI("http://example.org/a")
      val shape: SLang  = Ref(IRILabel(IRI("http://example.org/User")))
      val r: EitherT[IO,String,ShapesMap] = for {
        rdf <- io2es(RDFAsJenaModel.fromChars(
          """|<a> <name> "a" ;
             | <knows> <a> .
            |
          """.stripMargin, "TURTLE",Some(IRI("http://example.org/"))))
        schema <- io2es(Schema.fromString(
          """|
             |<User> {
             | <name> . ;
             | <knows> .
             |}
          """.stripMargin, "ShEXC",Some(IRI("http://example.org/"))))
        slangSchema <- shex2SLang(schema)
        eitherResult <- io2es(Validation.runValidation(node, shape, rdf, slangSchema))
        result <- either2es(eitherResult)
      } yield result

      run_es(r).unsafeRunSync.fold(e => fail(s"Error: $e"), result => {
        result.isConforming(node, shape) should be(Conforms)
      })
    }
  }
}
