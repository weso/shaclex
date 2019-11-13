package es.weso.shex.validator

import es.weso.rdf.RDFReader
import es.weso.rdf.jena._
import es.weso.rdf.nodes._
import es.weso.shex._
import org.scalatest._

class ValidatorTest extends FunSpec with Matchers with EitherValues {

  describe("ShEx validator") {
    val shapeLabel = IRILabel(IRI("http://example.org/S"))
    val schema =
      Schema.empty.copy(shapes = Some(List(NodeConstraint.nodeKind(IRIKind, List()).addId(shapeLabel))))
    val rdfStr = """|prefix : <http://example.org/>
                    |:a :p :b .""".stripMargin

    it("Should not validate if label not found") {
      shouldNotValidate(IRI("http://example.org/a"), IRILabel(IRI("http://example.org/Unknown")), rdfStr, schema)
    }

    it("Should validate single node constraint") {
      shouldValidate(IRI("http://example.org/a"), shapeLabel, rdfStr, schema)
    }
  }

  def shouldValidate(node: RDFNode, label: ShapeLabel, rdfStr: String, schema: Schema): Unit = {
    val v = Validator(schema)
    val check: ShExChecker.Check[ShapeTyping] = v.checkNodeLabel(node, label)
    val reader : Either[String, RDFReader] = RDFAsJenaModel
      .fromChars(rdfStr, "TURTLE")

    val obtained = reader
      .map(rdf => ShExChecker.runCheck(check, rdf).toEither)
      .getOrElse(sys.error("Unexpected Left found in Either"))
      .fold(err => fail(s"Error validating: $err"), _.hasType(node, label))

    obtained should be(true)
  }


  def shouldNotValidate(node: RDFNode, label: ShapeLabel, rdfStr: String, schema: Schema): Unit = {
    val v = Validator(schema)
    val check: ShExChecker.Check[ShapeTyping] = v.checkNodeLabel(node, label)
    val reader: Either[String, RDFReader] = RDFAsJenaModel
      .fromChars(rdfStr, "TURTLE")

    val obtained = reader
      .map(rdf => ShExChecker.runCheck(check, rdf).toEither)
      .getOrElse(sys.error("Unexpected Left found in Either"))
      .fold(err => fail(s"Error validating: $err"), _.hasNoType(node, label))

    obtained should be(true)
  }
}
