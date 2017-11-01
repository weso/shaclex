package es.weso.shex.validator

import org.scalatest._
import es.weso.shex._
import es.weso.rdf.nodes._
import es.weso.rdf.jena._
import es.weso.shapeMaps.ShapeMap

import util._

class ShapeMapValidatorTest extends FunSpec with Matchers with EitherValues {

  describe("Simple Shape") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S { :p . }
        |:CanVote xsd:integer MinInclusive 18
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p :b .
         |:c :p 1 .""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S", ":a@:S,:b@!:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:c@:S", ":a@:S,:b@!:S,:c@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:a@:T", ":a@:S,:a@!:T")
    shouldValidateWithShapeMap(rdfStr, shexStr, "23@:CanVote", "23@:CanVote")
  }

  describe("Recursive shape") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S { :p @:S }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p :b .
         |:b :p :a .
         |:c :p :c .
         |:d :p 1 .""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S,:b@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:c@:S", ":a@:S,:b@:S,:c@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:c@:S,:d@:S", ":a@:S,:b@:S,:c@:S,:d@!:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":d@:S", ":d@!:S")
  }

  describe("Two recursive shapes") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S { :p @:T }
        |:T { :q @:S }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p :b .
         |:b :q :a .
         |:c :p :c .
         |:d :p 1 .""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S,:b@:T")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":b@:T", ":a@:S,:b@:T")
  }

  describe("Regular expressions") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p /\\d{2}/ }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p "23" .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A", ":a@:A")
  }
  describe("Shape with EXTRA") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S EXTRA :p { :p [ 1 ] }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1, 2 .
         |:b :p 1 .
         |:bad :p 2 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S", ":a@:S,:b@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:bad@:S", ":a@:S,:b@:S,:bad@!:S")
  }

  describe("Shape with EXTRA and CLOSED") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S CLOSED EXTRA :p {
        | :p [ 1 2 3];
        | :p [ 3 4 5]
        |}
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1, 3 .
         |:b :p 2, 5, 7 .
         |:bad1 :p 2 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:bad1@:S", ":a@:S,:b@:S,:bad1@!:S")
  }
  describe("Shape with inverse arcs") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S { ^:p @:T* }
        |:T { :q . }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:t1 :p :s1; :q "a" .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":s1@:S", ":s1@:S,:t1@:T")
  }

  def shouldValidateWithShapeMap(
    rdfStr: String,
    shexStr: String,
    shapeMapStr: String,
    expected: String): Unit = {
    it(s"Should validate ${shexStr} with ${rdfStr} and ${shapeMapStr} and result $expected") {
      val validate = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr, "Turtle")
        shex <- Schema.fromString(shexStr, "ShExC", None)
        shapeMap <- ShapeMap.fromString(shapeMapStr, None, rdf.getPrefixMap, shex.prefixMap)
        fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, shex.prefixMap)
        result <- Validator.validate(shex, fixedShapeMap, rdf)
        expectedShapeMap <- ShapeMap.parseResultMap(expected, None, rdf, shex.prefixMap)
        compare <- result.compareWith(expectedShapeMap)
      } yield compare
      validate match {
        case Left(msg) => fail(s"Error: $msg")
        case Right(v) => v should be(true)
      }
    }
  }

}
