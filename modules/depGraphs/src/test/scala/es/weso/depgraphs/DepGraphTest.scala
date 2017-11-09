package es.weso.depgraphs

import org.scalatest._

class DepGraphTest
  extends FunSpec
  with Matchers
  with EitherValues {

  describe("A Graph") {

    it("should be able to create empty graph") {
      val emptyGraph = DepGraph.empty[String]
      emptyGraph.nodes should contain theSameElementsAs Set()
    }

    it("Should add one element") {
      val emptyGraph = DepGraph.empty[String]
      emptyGraph.addNode("a").nodes should contain theSameElementsAs Set("a")
    }

    it("Should add one edge") {
      val g = DepGraph.empty[String].
        addPosEdge("a", "b").
        addNegEdge("a", "c")

      g.outEdges("a").right.value should contain theSameElementsAs
        Set((Pos, "b"), (Neg, "c"))
    }

    it("Should calculate if graph has neg cycles a-(+)->b, a-(-)->c: false") {
      val g = DepGraph.empty[String].
        addPosEdge("a", "b").
        addNegEdge("a", "c")
      g.containsNegCycle should be(false)
    }

    it("Should calculate if graph has neg cycles when it hasn't") {
      val g = DepGraph.empty[String].
        addPosEdge("a", "b").
        addNegEdge("a", "c").
        addPosEdge("b", "d").
        addPosEdge("d", "a")

      println(s"graph: $g")
      g.containsNegCycle should be(false)
    }

    it("Should calculate neg cycles") {
      val g = DepGraph.empty[String].
        addNegEdge("a", "b").
        addPosEdge("a", "c").
        addPosEdge("b", "d").
        addPosEdge("d", "a")
      val cycles = g.negCycles
      cycles should have size 1
      val cycle = cycles.head
      cycle should contain theSameElementsAs Set("a", "b", "d")
      g.containsNegCycle should be(true)
    }
  }

}

