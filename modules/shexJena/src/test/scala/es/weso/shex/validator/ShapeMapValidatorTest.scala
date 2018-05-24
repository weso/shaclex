package es.weso.shex.validator

import org.scalatest._
import es.weso.shex._
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

  describe("Language stem") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p [@es] }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p "Hola"@es .
         |:x :p "Hi"@en .
         |:y :p "Hi" .
         |:z :p 23 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A", ":a@:A")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:x@:A,:y@:A,:z@:A", ":a@:A,:x@!:A,:y@!:A,:z@!:A")
  }

  describe("IRI stem") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p  [ <http://example.org/> ~ ] }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p <http://example.org/hi> .
         |:b :p :x .
         |:x :p <http://other.org/hi> .
         |:y :p "Hi" .
         |:z :p 23 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A", ":a@:A")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:b@:A", ":a@:A,:b@:A")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:b@:A,:x@:A,:y@:A,:z@:A", ":a@:A,:b@:A,:x@!:A,:y@!:A,:z@!:A")
  }
  describe("Closed list") {
    val shexStr =
      """
        |prefix : <http://example.org/>
		|PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |:A { :p  @:List }
		|:List CLOSED { 
		|  rdf:first @:B ;
		|  rdf:rest [rdf:nil] OR @:List
		|}
		|:B { :q . }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
     |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     |:a :p (:b1 :b2) .
		 |:b1 :q 1 .
     |:b2 :q 2 .
     |:ls rdf:first :b1; rdf:rest rdf:nil.
     |:x :p (:b1 :c1) .
     |:c1 :r 1 .
     |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":b1@:B", ":b1@:B")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":ls@:List", ":ls@:List,:b1@:B")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:ls@:List", ":a@:A,:ls@:List,:b1@:B,:b2@:B")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:x@:A,:ls@:List", ":a@:A,:ls@:List,:b1@:B,:b2@:B,:x@!:A")
  }
  describe("Labeled triple constraints") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { $<lbl> (:p .; :q .) }
        |:B { :r . ; &<lbl> }
        |""".stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1 ; :q 1 .
     		 |:b :p 1 ; :q 1; :r 1 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A", ":a@:A")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:b@:B", ":a@:A,:b@:B")
  }
  describe("Relative uris with base") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |base <http://base.org/>
        |<A> { :p . }
        |""".stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@<A>", ":a@<http://base.org/A>")
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
        shapeMap <- ShapeMap.fromCompact(shapeMapStr, None, rdf.getPrefixMap, shex.prefixMap)
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
