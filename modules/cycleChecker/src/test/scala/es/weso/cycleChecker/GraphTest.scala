package es.weso.cycleChecker
import org.scalatest._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scalax.collection.edge.LDiEdge // labeled directed edge
import scalax.collection.edge.Implicits._ // shortcuts
// import scalax.collection.connectivity._

class GraphTest extends FunSpec with Matchers {
  it("Should be able to create a graph") {
    val g = Graph("A", "A" ~> "B" % 2, "B" ~> "C" % 3, "C" ~> "A" % 3)
    println(g)
    println("Is cyclic: " + g.isCyclic)
  }
}