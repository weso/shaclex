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

  def negCyclesTest(schema: String, negCyclesLabels: Set[Set[String]]): Unit = {
    it(s"should check that negCycles of $schema are $negCyclesLabels") {
      Schema.fromString(schema, "SHEXC")
      match {
        case Failure(e) => fail(s"Error $e parsing $schema")
        case Success(schema) => {
          schema.negCycles match {
            case Left(msg) => fail(s"Error $msg calculating negative cycles of $schema")
            case Right(cycles) => (negCyclesLabels.isEmpty, cycles.isEmpty) match {
              case (true, true) => info("No neg cycles as expected")
              case (true, false) => {
//                val showCycles = cycles.map(_.map(_.id.map(_.toString).getOrElse("?")))
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


describe("Dependencies") {
   it("Neg cycles works ok when no cycles") {
     val strSchema =
       """
         |prefix : <http://example.org/>
         |
         |:S { :p @:T }
         |:T { :p @:S }
       """.stripMargin

     Schema.fromString(strSchema, "SHEXC") match {
       case Failure(e) => fail(s"Error $e parsing $strSchema")
       case Success(schema) => {
         Dependencies.negCycles(schema) match {
           case Left(e) => fail(s"Error $e calculating negCycles of $strSchema")
           case Right(nc) => {
             nc should be(Set())
           }
         }
       }
     }
   }

   negCyclesTest(
     """
         |prefix : <http://example.org/>
         |
         |:S NOT @:T
         |:T NOT @:S
       """.stripMargin,
     Set(Set("http://example.org/S","http://example.org/T"))
   )

 }
}
