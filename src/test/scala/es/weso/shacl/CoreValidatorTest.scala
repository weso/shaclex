package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import util._

class CoreValidatorTest extends 
  FunSpec with Matchers with TryValues with OptionValues 
  with SchemaMatchers {
  
describe("Core validator scope Nodes") {

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
    
    CoreValidator(schema).scopeNodes should contain only ((x,s),(y,s),(z,t))
  }

  it("should be able to validate minCount") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; 
                 |   sh:scopeNode :x ;
                 |   sh:property [ sh:predicate :p ;
                 |                 sh:minCount 1
                 |               ] .
                 |:x :p 1 .
                 |:good1 :p 1 .
                 |:good2 :p 1, 2 .
                 |:bad1 :q 1 .
                 |""".stripMargin
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (rdf,schema)
    val (rdf,schema) = attempt.success.value
    val S = ex + "S"
    val p = ex + "p"
    val x = ex + "x"
    val bad1 = ex + "bad1"
    val pc = PropertyConstraint(id = None,predicate = p,
            components= Seq(MinCount(1)))
    val s = Shape.empty.copy(
        id = Some(S), 
        scopes = Seq(ScopeNode(x)),
        components = Seq(pc))
    val validator = CoreValidator(schema)
    validator.scopeNodes should contain only ((x,s))
    validator.minCount(1).validate(x,(rdf,p)).isOK should be(true)
    val validated = validator.minCount(1).validate(bad1,(rdf,p))
    validated.isOK should be (false)
    validated.errors should contain (ViolationError.minCountError(bad1,p,1,0))
    validator.vPropertyConstraint(pc).validate(ex + "good1",rdf).isOK should be(true)
    validator.vPropertyConstraint(pc).validate(ex + "good2",rdf).isOK should be(true) 
  }
}

describe("minCount") {
  
}
}