package es.weso.shex

import es.weso.depgraphs.{DepGraph, Neg, Pos, PosNeg}
import es.weso.rdf.nodes._
import org.scalatest._

class dependenciesTest extends FunSpec with Matchers with EitherValues {

  // Some common values
  val e = "http://example.org/"
  val r: ShapeLabel = IRILabel(IRI(e + "R"))
  val s: ShapeLabel = IRILabel(IRI(e + "S"))
  val t: ShapeLabel = IRILabel(IRI(e + "T"))
  val u: ShapeLabel = IRILabel(IRI(e + "U"))

  ignore("Dependency graph") {

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


    depGraphTest(
      s"""prefix : <$e>
         |:S { :p NOT @:T }
         |:T NOT @:U
         |:U { :b @:S }
     """.stripMargin,
      Set(
        (s, Set((Neg, t))),
        (t, Set((Neg, u))),
        (u, Set((Pos, s)))
      ))


    depGraphTest(
      s"""prefix : <$e>
         |:S EXTRA :p { :p @:T }
     """.stripMargin,
      Set((s, Set((Pos, t), (Neg, t)))))

  }

  describe("Negative cycles test") {

    def negCyclesTest(schemaStr: String, negCyclesLabels: Set[Set[(ShapeLabel,ShapeLabel)]]): Unit = {
      it(s"should check that negCycles of schema: \n$schemaStr are:\n$negCyclesLabels") {
        Schema.fromString(schemaStr, "SHEXC", None) match {
          case Left(e) => fail(s"Error $e parsing $schemaStr")
          case Right(schema) => {
            schema.oddNegCycles match {
              case Left(msg) => fail(s"Error $msg calculating negative cycles of $schema")
              case Right(cycles) =>
                (negCyclesLabels.isEmpty, cycles.isEmpty) match {
                  case (true, true) => info("No odd neg cycles as expected")
                  case (true, false) => {
                    //                val showCycles = cycles.map(_.map(_.id.map(_.toString).getOrElse("?")))
                    val graphStr = schema.depGraph.map(_.showEdges()).getOrElse("<empty>")
                    info(s"Dependency graph = ${graphStr}")
                    fail(s"Expected no negCycles but found neg cycles: \n$cycles")
                  }
                  case (false, true) => {
                    val graphStr = schema.depGraph.map(_.showEdges()).getOrElse("<empty>")
                    info(s"Dependency graph = ${graphStr}")
                    fail(s"Expected negCycles to be \n$negCyclesLabels\n but found no neg cycles")
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

/*    negCyclesTest(
      """prefix : <http://example.org/>
        |:S { :p @:T } AND NOT @:R
        |:R { :q @:S }
        |:T { :p @:S }
      """.
        stripMargin, Set(Set((s, r)))) */

    negCyclesTest(
      s"""prefix : <$e>
         |:S { :p NOT @:T }
         |:T NOT @:U
         |:U { :b @:S }
     """.stripMargin,
      Set())

    negCyclesTest(
      s"""|PREFIX :       <http://example.org/>
          |:S EXTRA :a { :a @:T }
          |:T { :b . }
     """.stripMargin,
      Set())

    negCyclesTest(
      s"""|PREFIX :       <http://example.org/>
          |:S EXTRA :a { :a @:S }
     """.stripMargin,
      Set(Set((s,s))))

    negCyclesTest(
      s"""|PREFIX :       <http://example.org/>
          |:S { :a NOT @:T }
          |:T { :a NOT @:U }
          |:U { :a NOT @:S }
     """.stripMargin,
      Set(Set((s, t), (u,s), (t,u)))
    )

    negCyclesTest(
      s"""|PREFIX :       <http://example.org/>
          |:S { :a NOT @:T }
          |:T { :a NOT @:U }
          |:U { :a @:S }
     """.stripMargin,
      Set())

  }

}
