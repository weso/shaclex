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

    /*    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S", ":a@:S,:b@!:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:c@:S", ":a@:S,:b@!:S,:c@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:a@:T", ":a@:S,:a@!:T") */
    shouldValidateWithShapeMap(rdfStr, shexStr, "23@:CanVote", "23@:CanVote")
  }

  /*  describe("Recursive shape") {
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
*/
  def shouldValidateWithShapeMap(
    rdfStr: String,
    shexStr: String,
    shapeMapStr: String,
    expected: String): Unit = {
    it(s"Should validate ${shexStr} with ${rdfStr} and ${shapeMapStr} and result $expected") {
      val validate = for {
        rdf <- RDFAsJenaModel.parseChars(rdfStr, "Turtle")
        shex <- Schema.parse(shexStr, "ShExC", None)
        shapeMap <- ShapeMap.parse(shapeMapStr, rdf.getPrefixMap, shex.prefixMap)
        fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, shex.prefixMap)
        result <- Validator.validate(shex, fixedShapeMap, rdf)
        expectedShapeMap <- ShapeMap.parseResultMap(expected, rdf, shex.prefixMap)
        compare <- result.compareWith(expectedShapeMap)
      } yield compare
      validate match {
        case Left(msg) => fail(s"Error: $msg")
        case Right(v) => v should be(true)
      }
    }
  }

}