package es.weso.slanguage


import es.weso.slanguage.Clingo._
import org.scalatest._
import java.io._


class ClingoTest extends FunSpec with Matchers with TryValues with OptionValues {
  describe(s"ClingoTest") {
    it(s"Should show a program") {
    val program: Program = Program(
        List(
          ShowDirective("hasShape", 2),
          Fact(Pos(Function(Func("arc", List(Const("alice"), Const("knows"), Const("bob")))))),
          Rule(Function(Func("hasShape", List(Var("X"), Const("user")))),
            List(Pos(Function(Func("arc", List(Var("X"), Const("knows"), Var("Y")))))))
        ))

     val str = program.show
     writeContents("modules/converter/target/test.pl", program.show)
     info(s"Program: ${str}")
     str.isEmpty should be(false)
    }
  }

  def writeContents(fileName: String, contents: String): Unit = {
    val pw = new PrintWriter(new File(fileName))
    try { pw.write(contents) }
    finally { pw.close }
  }
}
