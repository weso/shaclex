package es.weso.shex.validator

import org.scalatest._
import es.weso.shex._
import es.weso.rdf.nodes._
import es.weso.rdf.jena._
import util._

class ValidatorTest extends FunSpec with Matchers with EitherValues {

  describe("ShEx validator") {
    val shapeLabel = IRILabel(IRI("http://example.org/S"))
    val schema =
      Schema.empty.copy(shapes = Some(List(
        NodeConstraint.nodeKind(IRIKind, List()).addId(shapeLabel)
       )))
    val rdfStr = """|prefix : <http://example.org/>
                    |:a :p :b .""".stripMargin

    it("Should not validate if label not found") {
      shouldNotValidate(IRI("http://example.org/a"), IRILabel(IRI("http://example.org/Unknown")), rdfStr, schema)
    }

    it("Should validate single node constraint") {
      shouldValidate(IRI("http://example.org/a"), shapeLabel,rdfStr,schema)
    }
  }

  def shouldValidate(
    node: RDFNode,
    label: ShapeLabel,
    rdfStr: String,
    schema: Schema): Unit = {
    val v = Validator(schema)
    RDFAsJenaModel.fromChars(rdfStr,"TURTLE") match {
      case Failure(e) => fail(s"Can't parse $rdfStr as RDF. Error: $e")
      case Success(rdf) => {
        val check = v.checkNodeLabel(node, label)
        val r = ShExChecker.runCheck(check, rdf)
        if (r.isOK) info("OK")
        else {
          fail(s"Failed\n$r")
        }
      }
    }
 }

 def shouldNotValidate(
    node: RDFNode,
    label: ShapeLabel,
    rdfStr: String,
    schema: Schema): Unit = {
    val v = Validator(schema)
    RDFAsJenaModel.fromChars(rdfStr,"TURTLE") match {
      case Failure(e) => fail(s"Can't parse $rdfStr as RDF. Error: $e")
      case Success(rdf) => {
        val check = v.checkNodeLabel(node, label)
        val r = ShExChecker.runCheck(check, rdf)
        if (r.isOK) fail(s"Validated when expected to fail\n$r")
        else {
          info(s"Failed as expected\n$r")
        }
      }
    }
 }}
