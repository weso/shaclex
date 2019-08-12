package es.weso.shacl.validator

import org.scalatest._
import SHACLChecker._
// import cats._
// import cats.implicits._
import es.weso.rdf.RDFReader
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.shacl.{MessageMap, Shape, RefNode}
import es.weso.shacl.report.{Severity, ValidationResult}
// import es.weso.shacl.validator.ShapeTyping._
// import es.weso.typing.Typing

class SHACLCheckerTest extends FunSpec with Matchers with TryValues with OptionValues {

  def mkShape(x: String): Shape = Shape.empty(StringLiteral(x))
  val rdf: RDFReader = RDFAsJenaModel.empty
  val emptyTyping: ShapeTyping = ShapeTyping.empty

  describe(s"checkAllTyping") {


    def mkErr(str: String): ValidationResult =
      ValidationResult.basic("",StringLiteral(str),Attempt(StringLiteral(str),
        RefNode(StringLiteral(str)),
        MessageMap.empty,
        Severity.defaultSeverity,None
      ),str)

    type T = (RDFNode, Shape, Boolean)

    def chk(v: T): CheckTyping = {
      val (node,shape,flag) = v
      for {
        t <- getTyping
      } yield if (flag)
        (t.addType(node,shape), true)
      else
        (t.addNotEvidence(node,shape,mkErr("false")), false)
    }

    shouldCheckAllTyping("checkAll x/1, true",LazyList((StringLiteral("x"),mkShape("1"),true)), chk, rdf, emptyTyping,
      (emptyTyping.addType(StringLiteral("x"),mkShape("1")),true)
    )

    shouldCheckAllTyping("checkAll x/1, x/2, true",
      LazyList((StringLiteral("x"),mkShape("1"),true),
             (StringLiteral("x"),mkShape("2"),true)), chk, rdf, emptyTyping,
      (emptyTyping.
        addType(StringLiteral("x"),mkShape("1")).
        addType(StringLiteral("x"),mkShape("2")),true)
    )

    shouldCheckAllTyping("checkAll x/1, x/not 2, false",
      LazyList((StringLiteral("x"),mkShape("1"),true),
        (StringLiteral("x"),mkShape("2"),false)), chk, rdf, emptyTyping,
      (emptyTyping.
        addType(StringLiteral("x"),mkShape("1")).
        addNotEvidence(StringLiteral("x"),mkShape("2"),mkErr("false")),false)
    )

    def shouldCheckAllTyping[A](msg: String, ls: LazyList[A], chk: A => CheckTyping, rdf: RDFReader, typing: ShapeTyping, expected: Result): Unit = {
      it(s"Should checkAllTyping $msg") {
        val (log, e) = SHACLChecker.run(checkAllTyping(ls,chk))(rdf)(typing)
        e.fold(e => fail(s"Error: $e"), r => {
          compare(r,expected)
        })
      }
    }
  }

  describe(s"combineResults") {

    val xs: ShapeTyping = ShapeTyping.empty.addType(StringLiteral("x"), mkShape("s"))

    shouldCombineResults("Combine empty typings", (emptyTyping,true),(xs,true),(xs,true))

    def shouldCombineResults(msg: String, r1: Result, r2: Result, expected: Result): Unit = {
      it(s"$msg") {
        val r = SHACLChecker.combineResults(r1,r2)
        compare(r, expected)
      }
    }
  }

  def compare(r1: Result, r2: Result): Unit = {
    (r1, r2) match {
      case ((t1, b1), (t2, b2)) => {
        b1 should ===(b2)
        t1.getNodes.foreach { node =>
          t1.getFailedValues(node) should contain theSameElementsAs (t2.getFailedValues(node))
        }
      }
    }
  }
}