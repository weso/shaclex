package es.weso.shex

import es.weso.depgraphs.{DepGraph, Neg, Pos, PosNeg}
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import org.scalatest._

class dependenciesTest extends FunSpec with Matchers with EitherValues {

  // Some common values
  val e = "http://example.org/"
  val r: ShapeLabel = IRILabel(IRI(e + "R"))
  val s: ShapeLabel = IRILabel(IRI(e + "S"))
  val t: ShapeLabel = IRILabel(IRI(e + "T"))

  describe("Dependency graph") {

    def depGraphTest(schema: String, expectedGraph: Set[(ShapeLabel, Set[(PosNeg, ShapeLabel)])]): Unit = {
      it(s"should check that dependency graph of $schema matches $expectedGraph") {
        val expectedDepGraph = DepGraph.makeGraph(expectedGraph)
        Schema.fromString(schema, "SHEXC",None) match {
          case Left(e) => fail(s"Error $e parsing $schema")
          case Right(schema) => {
            schema.depGraph match {
              case Left(msg) => fail(s"Error $msg calculating dependency graph")
              case Right(depGraph) => {
                depGraph.isomorphicWith(expectedDepGraph) match {
                  case Left(msg) => {
                    info(s"DepGraph obtained: ${depGraph.showEdges()}")
                    info(s"DepGraph expected: ${expectedDepGraph.showEdges()}")
                    fail(s"Graphs are not isomorphic: $msg")
                  }
                  case Right(()) => info(s"Graphs are isomorphic")
                }
              }
            }
          }
        }
      }
    }

    depGraphTest(
      s"""prefix : <$e>
       |:S { :p @:T }
     """.stripMargin, Set((s, Set((Pos, t)))))

    depGraphTest(
      s"""prefix : <$e>
      |:S { :p @:S }
     """.stripMargin, Set((s, Set((Pos, s)))))

    depGraphTest(
      s"""prefix : <$e>
         |:S NOT { :p @:S }
     """.stripMargin, Set((s, Set((Neg, s)))))

    depGraphTest(
      s"""prefix : <$e>
         |:S { :p @:T; :q NOT @:R }
     """.stripMargin,
      Set((s, Set((Pos, t), (Neg, r)))))

  }

  describe("Negative cycles test") {

    def negCyclesTest(schemaStr: String, negCyclesLabels: Set[Set[ShapeLabel]]): Unit = {
      it(s"should check that negCycles of $schemaStr are $negCyclesLabels") {
        Schema.fromString(schemaStr, "SHEXC",None) match {
          case Left(e) => fail(s"Error $e parsing $schemaStr")
          case Right(schema) => {
            schema.negCycles match {
              case Left(msg) => fail(s"Error $msg calculating negative cycles of $schema")
              case Right(cycles) => (negCyclesLabels.isEmpty, cycles.isEmpty) match {
                case (true, true) => info("No neg cycles as expected")
                case (true, false) => {
                  //                val showCycles = cycles.map(_.map(_.id.map(_.toString).getOrElse("?")))
                  val graphStr = schema.depGraph.map(_.showEdges()).getOrElse("<empty>")
                  info(s"Dependency graph = ${graphStr}")
                  fail(s"Expected no negCycles but found neg cycles: $cycles")
                }
                case (false, true) => {
                  val graphStr = schema.depGraph.map(_.showEdges()).getOrElse("<empty>")
                  info(s"Dependency graph = ${graphStr}")
                  fail(s"Expected negCycles to be $negCyclesLabels but found no neg cycles")
                }
                case (false, false) => {
                  cycles should contain theSameElementsAs (negCyclesLabels)
                }
              }
            }
          }
        }
      }
    }

    negCyclesTest(
      """
      |prefix : <http://example.org/>
      |
      |:S { :p @:T }
      |:T { :p @:S }
    """.
        stripMargin, Set())

    negCyclesTest(
      """prefix : <http://example.org/>
      |:S { :p @:T } AND NOT @:R
      |:T { :p @:S }
    """.
        stripMargin, Set())

    negCyclesTest(
      """prefix : <http://example.org/>
        |:S { :p @:T } AND NOT @:R
        |:R { :q @:S }
        |:T { :p @:S }
      """.
        stripMargin, Set(Set(r, s, t)))
  }

}
