package es.weso.rbe.matcher

import es.weso.rbe.StringGraph._
import es.weso.typing.PosNegTyping
import org.scalatest._
import scala.util._
import es.weso.rbe._

class MatchingSchemaTestSingle extends FunSpec with Matchers with TryValues {
  
  def noTs: Set[(String,String,String)] = Set()

  describe("Schema with ref") {

    val g = GraphMap(
      Map("n0" -> List(("a", "n1")),
          "n1" -> List(("b", "n1"), ("c", "n2"))))

    val s: Schema[String, String, String] =
      Schema(
        Map("t0" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("a"), Ref("t1"))), 1, 1)),
          "t1" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("b"), Ref("t2"))), 1, 1), Symbol(((DirectEdge("c"), Ref("t3"))), 1, 1))),
          "t2" -> Shape.empty.copy(rbe = And(Symbol(((DirectEdge("b"), Ref("t2"))), 0, 1), Symbol(((DirectEdge("c"), Ref("t3"))), 1, 1))),
          "t3" -> Shape.empty.copy(rbe = Empty)),
        Seq())

    val typing0 = PosNegTyping.fromPosMap(
        Map("n1" -> Set("t1", "t2"),
            "n2" -> Set("t3")))
    val expected0 = Seq((typing0,Set(("n1","c","n2"),("n1","b","n1"))))


    val typing1 =
      PosNegTyping.fromPosMap(
        Map("n0" -> Set("t0"),
            "n1" -> Set("t1", "t2"),
            "n2" -> Set("t3")))
    val expected1 = Seq((typing1,Set(("n1","b","n1"),("n0","a","n1"),("n1","c","n2"))))

      matchesNodeLabel("n1", "t1", g, s, expected0)

/*    it("should match n0 with t0") {
      matchesNodeLabel("n0", "t0", g, s, expected1)
    } */
      
  }

  
    def matchesNodeLabel[Edge, Node, Label](
      n: Node,
      l: Label,
      g: Graph[Edge, Node],
      s: Schema[Edge, Node, Label], 
      t: Seq[(PosNegTyping[Node, Label],Set[(Node,Edge,Node)])]): Unit = {
      val matcher = IterativeMatcher(s,g)
      it(s"Matches node $n with label $l in graph ${g} and schema ${s}") {
        matcher.matchNode(n, l) should be(Success(t))
      }
    }
    
    def noMatchNodeLabel[Edge, Node, Label](
      n: Node,
      l: Label,
      g: Graph[Edge, Node],
      s: Schema[Edge, Node, Label]): Unit = {
      val matcher = IterativeMatcher(s,g)
      it(s"Doesn't match node $n with label $l in graph ${g} and schema ${s}") {
        val result = matcher.matchNode(n, l) 
        result match {
          case Success(ls) if (!ls.isEmpty) => fail(s"It matches with: $ls")
          case _ => info("Doesn't match as expected")
        }
      }
    }

    def compareResults[A](s1: A, s2: A) = {
      if (s1 != s2) {
        fail(s"Values are different\n$s1\n$s2")
      }
    }

}