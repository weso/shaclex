package es.weso.slanguage


import es.weso.slanguage.Clingo._
import org.scalatest._
import java.io._

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI


class ClingoTest extends FunSpec with Matchers with TryValues with OptionValues {
  describe(s"ClingoTest") {
    it(s"Should show a program") {
    val program: Program = Program(
        List(
          ShowDirective("hasShape", 2),
          Fact(Pos(Function(Func("arc", List(Const("alice"), Const("knows"), Const("bob")))))),
          Rule(Pos(Function(Func("hasShape", List(Var("X"), Const("user"))))),
            List(Pos(Function(Func("arc", List(Var("X"), Const("knows"), Var("Y")))))))
        ))

     val str = program.show
     // writeContents("modules/converter/target/test.pl", program.show)
     info(s"Program: ${str}")
     str.isEmpty should be(false)
    }

    it(s"Should ground simple shape and simple RDF") {
      val strRDF =
        """|<a> <b> <c> .
          |
        """.stripMargin
      val shape : SLang = And(STrue,STrue)
      val r = for {
        rdf <- RDFAsJenaModel.fromChars(strRDF, "TURTLE", None)
      } yield rdf
      r.fold(e => fail(s"Error: $e"),values => {
        val rdf = values
        val program = Validation.ground(IRI("a"),shape, rdf)
        val contents = program.show
        writeContents("modules/converter/target/test.pl", contents)
        info(s"Contents written: $contents")
      }
      )
    }
  }

  def writeContents(fileName: String, contents: String): Unit = {
    val pw = new PrintWriter(new File(fileName))
    try { pw.write(contents) }
    finally { pw.close }
  }
}
