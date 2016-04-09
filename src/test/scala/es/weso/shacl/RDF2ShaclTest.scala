package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import util._

class RDF2ShaclTest extends FunSpec with Matchers {
  
describe("RDf2Shacl Syntax") {
  it("should be able to get the list of shapes") {
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape .
                 |""".stripMargin
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (rdf,schema,pm)
    attempt match {
      case Success((rdf,schema,pm)) => {
        schema.shapes.length should be(1)
      }
      case Failure(e) => fail(e.getMessage)
    }
  }
  
  }
}