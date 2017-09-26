package es.weso.shex.validator

import es.weso.rdf.jena._
import es.weso.rdf.nodes._
import es.weso.shex._
import org.scalatest._

import scala.util._

class ValidatorTest extends FunSpec with Matchers with EitherValues {

  describe("ShEx validator") {
    val shapeLabel = IRILabel(IRI("http://example.org/S"))
    val schema =
      Schema.empty.copy(shapes = Some(List(
        NodeConstraint.nodeKind(IRIKind, List()).addId(shapeLabel))))
    val rdfStr = """|prefix : <http://example.org/>
                    |:a :p :b .""".stripMargin

    it("Should not validate if label not found") {
      shouldNotValidate(IRI("http://example.org/a"), IRILabel(IRI("http://example.org/Unknown")), rdfStr, schema)
    }

    it("Should validate single node constraint") {
      shouldValidate(IRI("http://example.org/a"), shapeLabel, rdfStr, schema)
    }
  }

  def shouldValidate(
    node: RDFNode,
    label: ShapeLabel,
    rdfStr: String,
    schema: Schema): Unit = {
    val v = Validator(schema)
    val eitherResult = for {
      rdf <- RDFAsJenaModel.parseChars(rdfStr, "TURTLE")
      check = v.checkNodeLabel(node, label)
      result <- ShExChecker.runCheck(check, rdf).toEither
    } yield result
    eitherResult.fold(
      err => fail(s"Error validating: $err"),
      _.hasType(node, label) should be(true))
  }

  def shouldNotValidate(
    node: RDFNode,
    label: ShapeLabel,
    rdfStr: String,
    schema: Schema): Unit = {
    val v = Validator(schema)
    val eitherResult = for {
      rdf <- RDFAsJenaModel.parseChars(rdfStr, "TURTLE")
      check = v.checkNodeLabel(node, label)
      shapeTyping <- ShExChecker.runCheck(check, rdf).toEither
    } yield shapeTyping
    eitherResult.fold(
      err => fail(s"Error validating: $err"),
      _.hasNoType(node, label) should be(true))
  }
}
