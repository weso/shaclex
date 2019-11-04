package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import util._
import es.weso.shacl.converter.RDF2Shacl
import es.weso.shacl.validator.Validator
import cats.implicits._

class ShapeValidatorTest extends FunSpec with Matchers with TryValues with EitherValues {

  describe("Shapes") {
    it("Should validate single shape") {
      val ex = IRI("http://example.org/")
      val s = ex + "S"
      val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |@prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                 |
                 |:S a sh:Shape;
                 |   sh:targetNode :x;
                 |   sh:property [sh:path :p; sh:datatype xsd:string; sh:minCount 1] ;
                 |   sh:property [sh:path :q; sh:datatype xsd:integer; sh:minCount 1] .
                 |:x :p 23; :q "xx" .
                 |""".stripMargin
      val attempt = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
        shape <- schema.shape(s)
        validator = Validator(schema)
        result <- Validator.validate(schema, rdf).leftMap(_.toString)
      } yield (Validator(schema).showResult(result))
      attempt match {
        case Right(result) => {
          info(s"${result}")
        }
        case Left(e) => fail(s"Failed: $e")
      }
    }
  }
}
