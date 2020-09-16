package es.weso.slang

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shex.Schema
import org.scalatest.matchers.should._
import org.scalatest.funspec._
import es.weso.utils.IOUtils._
import cats.data._ 
import cats.effect._

class ValidateNDTest extends AnyFunSpec with Matchers with SLang2Clingo with ShEx2SLang {

  describe(s"SLang validation") {
    it(s"Should validate simple example") {
      val node = IRI("http://example.org/a")
      val x = IRI("http://example.org/x")
      val shape: SLang  = Ref(IRILabel(IRI("User")))
      val r = for {
        rdf <- RDFAsJenaModel.fromChars(
          """|<a> <x> "a", 1 .
          """.stripMargin, "TURTLE", Some(IRI("http://example.org/")))
        schema = SchemaS(Map(IRILabel(IRI("User")) -> QualifiedArc(Pred(x), SLang.string, Card(1,IntMax(1)))))
        result <- ValidateND.runValidation(node, shape, rdf, schema)
      } yield result

      r.unsafeRunSync.fold(e => fail(s"Error: $e"), result => {
        result.isConforming(node, shape) should be(Conforms)
      })
    }
  }

  describe(s"SLang validation with conversion from ShEx") {
    it(s"Should validate simple example") {
      val node = IRI("a")
      val shape: SLang  = Ref(IRILabel(IRI("User")))
      val r: IO[ShapesMap] = for {
        rdf <- RDFAsJenaModel.fromChars(
          """|<a> <x> 1 .
             |
          """.stripMargin, "TURTLE")
        schema <- Schema.fromString(
          """|
             |<User> {
             | <x> @<User>
             |}
          """.stripMargin, "ShEXC")
        slangSchema <- shex2SLang(schema)
        eitherResult <- ValidateND.runValidation(node, shape, rdf, slangSchema)
        result <- fromES(eitherResult)
      } yield result

      r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), result => {
        result.isConforming(node, shape) should be(Conforms)
      })
    }
  }

}
