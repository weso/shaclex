package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import util._
import Validator._

class ShapeValidatorTest extends
  FunSpec with Matchers with TryValues with OptionValues {

describe("Shapes") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:scopeNode :x;
                 |   sh:property [sh:predicate :p; sh:minCount 1] .
                 |:x :p "a" .
                 |""".stripMargin
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (rdf,schema)
    val (rdf,schema) = attempt.success.value
    val s = ex + "S"
    val validator = Validator(schema)
    val shape = schema.shape(s).value
    val checker = validator.shapeChecker(shape)
//    val result = checker.
//    result.isRight should be(true)
 }
}
