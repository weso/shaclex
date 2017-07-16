package es.weso.collection

import org.scalatest._
import org.scalatest.prop._

class GraphTest 
  extends FunSpec 
  with Matchers 
  with Checkers {

  describe("A Graph") {

    it("Should add one element") {
      val emptyGraph: Graph[String,String] = Graph.empty
      emptyGraph.insert("a").nodes should be("a")
    }

  }

}
