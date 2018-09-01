package es.weso.slang


import es.weso.slang.Clingo._
import org.scalatest._
import java.io._

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shapeMaps.ShapeMap


class ClingoTest extends FunSpec
  with Matchers with SLang2Clingo {

  describe(s"ClingoTest") {
    it(s"Should show a program") {
    val program: Program = Program(
        List(
          ShowDirective("hasShape", 2),
          Fact(Pos(Function(Func("arc", Const("alice"), Const("knows"), Const("bob"))))),
          Rule(Lit(Pos(Function(Func("hasShape", Var("X"), Const("user"))))),
            Pos(Function(Func("arc", Var("X"), Const("knows"), Var("Y")))))
        ))

     val str = program.show
     // writeContents("modules/converter/target/test.pl", program.show)
     info(s"Program: ${str}")
     str.isEmpty should be(false)
    }

    it(s"Should ground simple shape and simple RDF") {
      val strRDF =
        """|<alice> <name> "A" ;
           |        <knows> <alice> .
        """.stripMargin

      val schema: SchemaS = SchemaS(Map(
        IRILabel(IRI("user")) ->
          And(QualifiedArc(PredSet(Set(IRI("name"))), STrue,Card.one),
              QualifiedArc(PredSet(Set(IRI("knows"))), STrue,Card.oneStar))
      ))

      val r = for {
        rdf <- RDFAsJenaModel.fromChars(strRDF, "TURTLE", None)
        smap <- ShapeMap.empty.add(IRI("alice"), es.weso.shapeMaps.IRILabel(IRI("user")))
        program <- validate2Clingo(smap, rdf, schema)
      } yield (rdf,program)
      r.fold(e => fail(s"Error: $e"),values => {
        val (rdf,prog) = values
        val contents = prog.show
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
