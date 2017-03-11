package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import es.weso.rdf.path.{InversePath, PredicatePath}

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
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val s = ex + "S"
    val t = ex + "T"
    info(s"Attempt: $attempt")
    attempt.success.value should constainShapes(Set(s,t))
  }

  it("should be able to get the list of target nodes") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:targetNode :n1 .
                 |:T a sh:Shape .
                 |""".stripMargin
    val s = ex + "S"
    val t = ex + "T"
    val n1 = ex + "n1"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (schema.shape(s))
    val maybeShape = attempt.success.value
    maybeShape shouldBe defined
    val targetNodes = maybeShape.get.targetNodes
    targetNodes should contain only(n1)
  }

  it("should be able to get the target node declarations") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:targetNode :s1, :s2 .
                 |:T a sh:Shape; sh:targetNode :t1 .
                 |""".stripMargin
    val S = ex + "S"
    val T = ex + "T"
    val s1 = ex + "s1"
    val s2 = ex + "s2"
    val t1 = ex + "t1"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val schema = attempt.success.value
    schema.targetNodeDeclarations should contain only((s2,S), (s1,S),(t1,T))
  }

    it("should be able to get the some property constraints") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape;
                 |   sh:property [
                 |     sh:path :p;
                 |     sh:nodeKind sh:IRI
                 |     ] .
                 |""".stripMargin
    val S = ex + "S"
    val p = ex + "p"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val schema = attempt.success.value
    val shape = schema.shape(S).value
    val p1 = PropertyShape(
        id = None,
        path = PredicatePath(p),
        components = Seq(NodeKind(IRIKind)))
    shape.propertyConstraints should contain only(p1)
  }

    it("should be able to get the property constraint with cardinalities") {
    val ex = IRI("http://example.org/")
    val str = """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape;
                 |   sh:property [
                 |    sh:path :p;
                 |    sh:nodeKind sh:IRI;
                 |    sh:minCount 1;
                 |    sh:maxCount 1
                 |    ] .
                 |""".stripMargin
    val S = ex + "S"
    val p = ex + "p"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val schema = attempt.success.value
    val shape = schema.shape(S).value
    val p1 = PropertyShape(
      id = None,
      path = PredicatePath(p),
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
    val ex = "http://example.org/"
    val str = s"""|prefix : <$ex>
                 |prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape;
                 |   sh:property [
                 |    sh:path :p;
                 |    sh:minCount 1
                 |   ] .
                 |""".stripMargin
    val S = IRI(ex) + "S"
    val p = IRI(ex) + "p"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val schema = attempt.success.value
    val shape = schema.shape(S).value
    shape.propertyConstraints.length should be(1)
    val pc = shape.propertyConstraints.head
    pc.id should be(None)
    pc.predicate should be(p)
    pc.components should contain only(MinCount(1))
  }

  it("should be able to get a path") {
    val ex = "http://example.org/"
    val str = s"""|prefix : <$ex>
                 |prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape;
                 |   sh:property [
                 |    sh:path [ sh:inversePath :p] ;
                 |    sh:minCount 1
                 |   ] .
                 |""".stripMargin
    val S = IRI(ex) + "S"
    val p = IRI(ex) + "p"
    val attempt = for {
      rdf : RDFReader <- RDFAsJenaModel.fromChars(str,"TURTLE")
      schema <- RDF2Shacl.getShacl(rdf)
    } yield (schema)
    val schema = attempt.success.value
    val shape = schema.shape(S).value
    val ip = InversePath(PredicatePath(p))
    shape.propertyConstraints.length should be(1)
    val pc = shape.propertyConstraints.head
    pc.id should be(None)
    pc.path should be(ip)
    pc.components should contain only(MinCount(1))
  }
}

}
