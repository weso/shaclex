package es.weso.shex

import cats._
import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.shex.implicits.decoderShEx._
import es.weso.shex.implicits.encoderShEx._
import es.weso.shex.implicits.eqShEx._
import es.weso.shex.implicits.showShEx._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest._

import scala.util.{Failure, Success}

class dependenciesTest extends FunSpec with Matchers with EitherValues {

  describe("Dependency graph") {

    def depGraphTest(schema: String, expectedGraph: Set[(String, Set[String])]): Unit = {
      it(s"should check that dependency graph of $schema matches $expectedGraph") {
        Schema.fromString(schema, "SHEXC") match {
          case Failure(e) => fail(s"Error $e parsing $schema")
          case Success(schema) => {
            schema.depGraph match {
              case Left(msg) => fail(s"Error $msg calculating dependency graph")
              case Right(depGraph) => {
                info(s"Dependency graph: ${depGraph.showEdges()}")
              }
            }
          }
        }
      }
    }
    val e = "http://example.org/"
    val s = e + "S"
    val t = e + "T"
   depGraphTest(
     s"""
       |prefix : <$e>
       |
       |:S { :p @:T }
     """.stripMargin, Set((s, Set(t))))
  }

  describe("Negative cycles test") {

    def negCyclesTest(schemaStr: String, negCyclesLabels: Set[Set[String]]): Unit = {
      val negCyclesExpected: Set[Set[IRILabel]] = negCyclesLabels.map((vs: Set[String]) => vs.map((s: String) => IRILabel(IRI(s))))
      it(s"should check that negCycles of $schemaStr are $negCyclesLabels") {
        Schema.fromString(schemaStr, "SHEXC")
        match {
          case Failure(e) => fail(s"Error $e parsing $schemaStr")
          case Success(schema) => {
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
                  cycles should contain theSameElementsAs (negCyclesExpected)
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
      """
      |prefix : <http://example.org/>
      |
      |:S { :p @:T } AND NOT @:R
      |:T { :p @:S }
    """.
        stripMargin, Set())

}

}
