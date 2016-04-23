package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import util._

class CoreValidatorTest extends 
  FunSpec with Matchers with TryValues with OptionValues 
  with SchemaMatchers {
  
describe("Core validator") {

  it("should be able to get the scope nodes to validate") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:scopeNode :x, :y .
                 |:T a sh:Shape; sh:scopeNode :z .
                 |""".stripMargin
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (rdf,schema)
    val (rdf,schema) = attempt.success.value
    val S = ex + "S"
    val T = ex + "T"
    val x = ex + "x"
    val y = ex + "y"
    val z = ex + "z"
    val s = Shape.empty.copy(id = Some(S), scopes = Seq(ScopeNode(y),ScopeNode(x)))
    val t = Shape.empty.copy(id = Some(T), scopes = Seq(ScopeNode(z)))
    
    CoreValidator(rdf,schema).scopeNodes should contain only ((x,s),(y,s),(z,t))
  }
   
}
}