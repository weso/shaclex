package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._

import util._
import Validator._
import es.weso.rdf.path.PredicatePath
import es.weso.shacl.converter.RDF2Shacl

class ValidatorTest extends
  FunSpec with Matchers with TryValues with OptionValues
  with SchemaMatchers {

describe("Validator target Nodes") {

  it("should be able to obtain the target nodes to validate") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:targetNode :x, :y .
                 |:T a sh:Shape; sh:targetNode :z .
                 |""".stripMargin
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (rdf,schema)
    info(s"attempt: $attempt")
    val (rdf,schema) = attempt.success.value
    val S = ex + "S"
    val T = ex + "T"
    val x = ex + "x"
    val y = ex + "y"
    val z = ex + "z"
    val s = Shape.empty.copy(id = Some(S), targets = Seq(TargetNode(x),TargetNode(y)))
    val t = Shape.empty.copy(id = Some(T), targets = Seq(TargetNode(z)))
    val targetNodes = Validator(schema).targetNodes
    targetNodes.size should be(3)
    targetNodes should contain (x,s)
    targetNodes should contain (y,s)
    targetNodes should contain (z,t)
  }

  it("should be able to validate minCount") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:NodeShape;
                 |   sh:targetNode :x ;
                 |   sh:property [ sh:path :p ;
                 |                 sh:minCount 1
                 |               ] .
                 |:x :p 1 .
                 |:good1 :p 1 .
                 |:good2 :p 1, 2 .
                 |:bad1 :q 1 .
                 |""".stripMargin
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (rdf,schema)
    val (rdf,schema) = attempt.success.value
    val S = ex + "S"
    val p = ex + "p"
    val x = ex + "x"
    val good1 = ex + "good1"
    val good2 = ex + "good2"
    val bad1 = ex + "bad1"
    val ps = Shape.emptyPropertyShape(PredicatePath(p)).copy(components=Seq(MinCount(1)))
    val s = Shape.empty.copy(
      id = Some(S),
      targets = List(TargetNode(x)),
      propertyShapes = Seq(ps)
    )
    val validator = Validator(schema)
    validator.targetNodes should contain only ((x,s))
    val checked = validator.validateAll(rdf)
    checked.isOK should be(true)
  }
}


describe("minCount") {
  it("validates minCount(1) when there is exactly 1") {
  val ex = IRI("http://example.org/")
  val str = s"""|@prefix : $ex
                |:x :p 1 .
                |""".stripMargin
    val rdf = RDFAsJenaModel.fromChars(str,"TURTLE").get
    val x = ex + "x"
    val p = ex + "p"
    val validator = Validator(Schema.empty)
    val checked = validator.validateAll(rdf)
    val s = Shape.empty
    checked.isOK should be(true)
  }
/*
 it("validates minCount(1) when there are 2") {
  val ex = IRI("http://example.org/")
  val str = s"""|@prefix : $ex
                |:x :p 1, 2 .
                |""".stripMargin
    val rdf = RDFAsJenaModel.fromChars(str,"TURTLE").get
    val x = ex + "x"
    val p = ex + "p"
    val validator = Validator(Schema.empty)
    validator.minCount(1).validateAll(x,(rdf,p)).isOK should be(true)
  }
 it("validates minCount(1) when there are 2 and other things...") {
  val ex = IRI("http://example.org/")
  val str = s"""|@prefix : $ex
                |:x :p 1, 2; :q 1 .
                |""".stripMargin
    val rdf = RDFAsJenaModel.fromChars(str,"TURTLE").get
    val x = ex + "x"
    val p = ex + "p"
    val validator = Validator(Schema.empty)
    validator.minCount(1).validateAll(x,(rdf,p)).isOK should be(true)
  }
 
  it("fails to validate minCount(2) when there are 1") {
  val ex = IRI("http://example.org/")
  val str = s"""|@prefix : $ex
                |:x :p 1; :q 1 .
                |""".stripMargin
    val rdf = RDFAsJenaModel.fromChars(str,"TURTLE").get
    val x = ex + "x"
    val p = ex + "p"
    val validator = Validator(Schema.empty)
    validator.minCount(2).validateAll(x,(rdf,p)).isOK should be(false)
  }

  it("fails to validate minCount(2) when there are no values") {
  val ex = IRI("http://example.org/")
  val str = s"""|@prefix : $ex
                |:x :q 1 .
                |""".stripMargin
    val rdf = RDFAsJenaModel.fromChars(str,"TURTLE").get
    val x = ex + "x"
    val p = ex + "p"
    val validator = Validator(Schema.empty)
    validator.minCount(2).validateAll(x,(rdf,p)).isOK should be(false)
  }
  
 it("fails to validate minCount(2) when there are no resource") {
  val ex = IRI("http://example.org/")
  val str = s"""|@prefix : $ex
                |:y :q 1 .
                |""".stripMargin
    val rdf = RDFAsJenaModel.fromChars(str,"TURTLE").get
    val x = ex + "x"
    val p = ex + "p"
    val validator = Validator(Schema.empty)
    validator.minCount(2).validateAll(x,(rdf,p)).isOK should be(false)
  }
*/
 }
/*
 describe("Property constraint"){
   it("validates minCount(1), maxCount(1) when there is exactly 1") {
     val ex = IRI("http://example.org/")
     val p = ex + "p"
     val x = ex + "x"
     val pc = PropertyConstraint(id = None, predicate = p, components = 
       Seq(MinCount(1),MaxCount(1)))
     val validator = Validator.empty
     val str = s"""|@prefix : $ex
                   |:x :p "a" .
                   |""".stripMargin
     val rdf = RDFAsJenaModel.fromChars(str,"TURTLE").get
     val validated = validator.vPropertyConstraint(pc).validateAll(x,rdf)
     validated.isOK should be(true)
   }
 }
 
 describe("MinCount shape") {
   val strSchema = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Constraint; sh:scopeNode :x;
                 |   sh:property [sh:predicate :p; sh:minCount 1] .
                 |""".stripMargin
   val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(strSchema,"TURTLE")
      (schema,pm) <- RDF2Shacl.getShacl(rdf)
    } yield (rdf,schema)
   val (rdf,schema) = attempt.get
   val ex = IRI("http://example.org/")
   val s = ex + "S"

   it("validates a single shape") {
   val str = """|@prefix : <http://example.org/>
                |@prefix sh: <http://www.w3.org/ns/shacl#>
                |
                |:x :p "a" .
                |:S sh:scopeNode :x .
                |""".stripMargin
    val rdf = RDFAsJenaModel.fromChars(str,"TURTLE").get
    val validator = Validator(schema)
    val shape = schema.shape(s).value
    val result = validator.shapeConstraint.validateAll(shape,rdf)
    result.isOK should be(true)
   }
    
  it("fails to validate a single shape which doesn't satisfy minCount") {
   val str = """|@prefix : <http://example.org/>
                |@prefix sh: <http://www.w3.org/ns/shacl#>
                |
                |:y :q "a" .
                |:S sh:scopeNode :y .
                |""".stripMargin
    val rdf = RDFAsJenaModel.fromChars(str,"TURTLE").get
    val validator = Validator(schema)
    val shape = schema.shape(s).value
    val result = validator.shapeConstraint.validate(shape,rdf)
    result.isOK should be(false)
   } 
 } */


}
