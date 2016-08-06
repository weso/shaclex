package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import util._

class RDF2ShaclTest extends 
  FunSpec with Matchers with TryValues with OptionValues 
  with SchemaMatchers {
  
describe("RDf2Shacl Syntax") {

  it("should be able to get the list of shapes") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape .
                 |:T a sh:Shape .
                 |""".stripMargin
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val s = ex + "S"
    val t = ex + "T"
    attempt.success.value should constainShapes(Set(s,t))
  }
  
  it("should be able to get the list of scope nodes") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:scopeNode :n1 .
                 |:T a sh:Shape .
                 |""".stripMargin
    val s = ex + "S"
    val t = ex + "T"
    val n1 = ex + "n1"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (schema.shape(s))
    val maybeShape = attempt.success.value
    maybeShape shouldBe defined
    val targetNodes = maybeShape.get.targetNodes
    targetNodes should contain only(n1)    
  }
  
  it("should be able to get the scope node declarations") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:scopeNode :s1, :s2 .
                 |:T a sh:Shape; sh:scopeNode :t1 .
                 |""".stripMargin
    val S = ex + "S"
    val T = ex + "T"
    val s1 = ex + "s1"
    val s2 = ex + "s2"
    val t1 = ex + "t1"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val schema = attempt.success.value
    schema.targetNodeDeclarations should contain only((s2,S), (s1,S),(t1,T))
  }
  
  
    it("should be able to get the some property constraints") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:property [ sh:predicate :p; sh:nodeKind sh:IRI ] .
                 |""".stripMargin
    val S = ex + "S"
    val p = ex + "p"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val schema = attempt.success.value
    val shape = schema.shape(S).value
    val p1 = PropertyConstraint(id = None, predicate = p, components = Seq(NodeKind(IRIKind))) 
    shape.propertyConstraints should contain only(p1)
  }

    it("should be able to get the property constraint with cardinalities") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:property [ 
                 |   sh:predicate :p; 
                 |   sh:nodeKind sh:IRI;
                 |   sh:minCount 1;
                 |   sh:maxCount 1 
                 |   ] .
                 |""".stripMargin
    val S = ex + "S"
    val p = ex + "p"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val schema = attempt.success.value
    val shape = schema.shape(S).value
    val p1 = PropertyConstraint(id = None, predicate = p, 
        components = Seq(
            NodeKind(IRIKind),
            MinCount(1),
            MaxCount(1))
        ) 
    shape.propertyConstraints.length should be(1)
    val pc = shape.propertyConstraints.head
    pc.id should be(None)
    pc.predicate should be(p)
    pc.components should contain only(NodeKind(IRIKind), MinCount(1), MaxCount(1))
  }
    
    it("should be able to get the property constraint with minCount cardinality only") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:property [ 
                 |   sh:predicate :p; 
                 |   sh:minCount 1
                 |   ] .
                 |""".stripMargin
    val S = ex + "S"
    val p = ex + "p"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val schema = attempt.success.value
    val shape = schema.shape(S).value
    val p1 = PropertyConstraint(id = None, predicate = p, 
        components = Seq(MinCount(1))
        ) 
    shape.propertyConstraints.length should be(1)
    val pc = shape.propertyConstraints.head
    pc.id should be(None)
    pc.predicate should be(p)
    pc.components should contain only(MinCount(1))
  }
    
} 
}