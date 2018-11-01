package es.weso.slang

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shex.Schema
import org.scalatest._

class ValidateNDTest extends FunSpec
  with Matchers with SLang2Clingo with ShEx2SLang {

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
      } yield (rdf,schema,schema)

      r.fold(e => fail(s"Error: $e"), values => {
        val (rdf,schema,slangSchema) = values
        val result = ValidateND.runValidation(node, shape, rdf, slangSchema)
        info(s"SLang schema: $slangSchema")
        info(s"Result for $node:\n${result.map(node).m.map(pair => { s"${pair._1}: ${pair._2}"}).mkString("\n")}")
        result.isConforming(node, shape) should be(Conforms)
      })
    }
  }

  describe(s"SLang validation with conversion from ShEx") {
    it(s"Should validate simple example") {
      val r = for {
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
      } yield (rdf,schema,slangSchema)

      r.fold(e => fail(s"Error: $e"), values => {
        val (rdf,schema,slangSchema) = values
        val node = IRI("a")
        val shape: SLang  = Ref(IRILabel(IRI("User")))
        val result = ValidateND.runValidation(node, shape, rdf, slangSchema)
        info(s"SLang schema: $slangSchema")
        info(s"Result for $node:\n${result.map(node).m.map(pair => { s"${pair._1}: ${pair._2}"}).mkString("\n")}")
        result.isConforming(node, shape) should be(Conforms)
      })
    }
  }

}
