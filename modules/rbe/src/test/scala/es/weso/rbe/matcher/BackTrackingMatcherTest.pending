package es.weso.rbe.matcher
import es.weso.rbe._
import es.weso.rbe.StringGraph._
import org.scalatest._

class BacktrackingMatcherTest extends FunSpec with Matchers {

  describe("Backtracking matcher") {
    
   describe("Fails to match is no label") {
    val s : Schema[String,String,String] =
      Schema(Map("t0" -> Shape.empty.copy(rbe = Symbol(((DirectEdge("a"), Ref("t0"))), 1, 1))),
             Seq())
    val g = GraphMap(Map("n0" -> List(("a", "n0"))))
    noMatchNodeLabel("n0", "t1", g, s)
   }
    
   describe("Node constraint") {
    val s : Schema[String,String,String] =
       Schema.empty.copy(
           m = Map("s" -> Shape.empty.copy(nodeShape = letter)),
           ignored = Seq())
    val g = GraphMap(Map("n0" -> List(("a", "n0")),
                         "ab" -> List()))
    noMatchNodeLabel("n0", "s", g, s)
    matchNodeLabel("ab", "s", g, s)
    }
  }
  
   describe("Not shape") {
    val s : Schema[String,String,String] =
       Schema.empty.copy(
           m = Map("noInt" -> NotShape(Shape.empty.copy(nodeShape = integer)) ),
           ignored = Seq())
    val g = GraphMap(Map("0" -> List(("a", "0")),
                         "a" -> List(("a", "a"))))
    noMatchNodeLabel("0", "noInt", g, s)
    matchNodeLabel("a", "noInt", g, s)
  }

   describe("AND shape") {
    val s : Schema[String,String,String] =
       Schema.empty.copy(
           m = Map("s" -> AndShape(
                                 Shape.empty.copy(nodeShape = letter),
                                 Shape.empty.copy(nodeShape = size2)) ),
           ignored = Seq())
    val g: Graph[String,String] = 
            GraphMap(Map("0" -> List(),
                         "aa" -> List(),
                         "aaa" -> List()))
    noMatchNodeLabel("0", "s", g, s)
    matchNodeLabel("aa", "s", g, s)
    noMatchNodeLabel("aaa", "s", g, s)
  }
   
  def matchNodeLabel[Edge, Node, Label](
      n: Node,
      l: Label,
      g: Graph[Edge, Node],
      s: Schema[Edge, Node, Label]): Unit = {
      it(s"Matches node $n with label $l in graph ${g} and schema ${s}") {
        val matcher = BacktrackingMatcher(s,g)
        val result = matcher.checkNodeLabel(n, l)
        info(result.value.toString)
        result.isOK should be(true)
      }
    }

  def noMatchNodeLabel[Edge, Node, Label](
      n: Node,
      l: Label,
      g: Graph[Edge, Node],
      s: Schema[Edge, Node, Label]): Unit = {
      it(s"Doesn't match node $n with label $l in graph ${g} and schema ${s}") {
        val matcher = BacktrackingMatcher(s,g)
        val result = matcher.checkNodeLabel(n, l)
        info(result.value.toString)
        result.isOK should be(false)
      }
    }
}