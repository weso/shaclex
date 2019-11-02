package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.converter.RDF2Shacl
import es.weso.rdf.path._
import util._

class RDF2ShaclTest extends FunSpec with Matchers with TryValues with EitherValues
  with SchemaMatchers {

  describe("RDf2Shacl Syntax") {

    it("should be able to get the a shape") {
      val ex = IRI("http://example.org/")
      val str =
        """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape .
                 |""".stripMargin
      val attempt: Either[String, Schema] = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
      } yield (schema)
      val s = ex + "S"
      info(s"Attempt: $attempt")
      attempt match {
        case Left(e) => fail(s"Failed: $e")
        case Right(v) => v should containShapes(Set(s))
      }
    }

    it("should be able to get the list of shapes") {
      val ex = IRI("http://example.org/")
      val str =
        """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape .
                 |:T a sh:Shape .
                 |""".stripMargin
      val attempt: Either[String, Schema] = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
      } yield (schema)
      val s = ex + "S"
      val t = ex + "T"
      info(s"Attempt: $attempt")
      attempt match {
        case Left(e) => fail(s"Failed: $e")
        case Right(v) => v should containShapes(Set(s, t))
      }
    }

    it("should be able to get the list of target nodes") {
      val ex = IRI("http://example.org/")
      val str =
        """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape; sh:targetNode :n1 .
                 |:T a sh:Shape .
                 |""".stripMargin
      val s = ex + "S"
      val n1 = ex + "n1"
      val attempt = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
        shape <- schema.shape(s)
      } yield (shape)
      attempt match {
        case Left(e) => fail(s"Failed $e")
        case Right(shape) => {
          shape.targetNodes should contain only (n1)
        }
      }
    }

    it("should be able to get the target node declarations") {
      val ex = IRI("http://example.org/")
      val str =
        """|@prefix : <http://example.org/>
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
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
      } yield (schema)
      attempt match {
        case Left(e) => fail(s"Failed $e")
        case Right(schema) => {
          schema.targetNodeDeclarations should contain only ((s2, S), (s1, S), (t1, T))
        }
      }
    }

    it("should be able to get some property constraints") {
      val ex = IRI("http://example.org/")
      val str =
        """|@prefix : <http://example.org/>
         |@prefix sh: <http://www.w3.org/ns/shacl#>
         |
         |:S a sh:Shape;
         |   sh:property :prop .
         |
         | :prop sh:path :p;
         |       sh:nodeKind sh:IRI .
         |""".stripMargin
      val S = ex + "S"
      val prop = ex + "prop"
      val attempt = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
        shape <- schema.shape(S)
      } yield (shape)
      //val p1 = Shape.emptyPropertyShape(prop, PredicatePath(p)).copy(components = Seq(NodeKind(IRIKind)))
      attempt match {
        case Left(e) => fail(s"Failed $e")
        case Right(shape) => {
          shape.propertyShapes should contain only (RefNode(prop))
        }
      }

    }

    it("should be able to get a property constraint with cardinalities") {
      val ex = IRI("http://example.org/")
      val str =
        """|@prefix : <http://example.org/>
                 |@prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape;
                 |   sh:property :prop .
                 |
                 |:prop sh:path :p;
                 |    sh:nodeKind sh:IRI;
                 |    sh:minCount 1;
                 |    sh:maxCount 1
                 |    .
                 |""".stripMargin
      val S = ex + "S"
      val p = ex + "p"
      val prop = ex + "prop"
      val attempt: Either[String, (Shape, Schema)] = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
        shape <- schema.shape(S)
      } yield ((shape, schema))

      attempt match {
        case Left(e) => fail(s"Failed $e")
        case Right((shape, schema)) => {
          shape.propertyShapes.length should be(1)
          val sref = shape.propertyShapes.head
          info(s"Shape ref: $sref")
          schema.shapesMap.get(sref) match {
            case Some(pc: PropertyShape) => {
              pc.id should be(prop)
              pc.predicate should be(Some(p))
              pc.components should contain only (NodeKind(IRIKind), MinCount(1), MaxCount(1))
            }
            case other => fail(s"Failed with $other")
          }
        }
      }
    }

    it("should be able to get the property constraint with minCount cardinality only") {
      val ex = "http://example.org/"
      val str = s"""|prefix : <$ex>
                 |prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape;
                 |   sh:property :prop .
                 |:prop sh:path :p;
                 |    sh:minCount 1 .
                 |""".stripMargin
      val S = IRI(ex) + "S"
      val p = IRI(ex) + "p"
      val prop = IRI(ex) + "prop"
      val attempt = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
        shape <- schema.shape(S)
      } yield (shape, schema)
      attempt match {
        case Left(e) => fail(s"Failed $e")
        case Right((shape, schema)) => {
          shape.propertyShapes.length should be(1)
          val sref = shape.propertyShapes.head
          schema.shapesMap.get(sref) match {
            case None => fail(s"Not found shape with ref $sref in $schema")
            case Some(ps: PropertyShape) => {
              ps.id should be(prop)
              ps.predicate should be(Some(p))
              ps.components should contain only (MinCount(1))
            }
            case other => fail(s"Unexpected value $other")
          }
        }
      }
    }

    it("should be able to get a path") {
      val ex = "http://example.org/"
      val str = s"""|prefix : <$ex>
                 |prefix sh: <http://www.w3.org/ns/shacl#>
                 |
                 |:S a sh:Shape;
                 |   sh:property :prop .
                 |:prop sh:path [ sh:inversePath :p ];
                 |    sh:minCount 1 .
                 |""".stripMargin
      val S = IRI(ex) + "S"
      val p = IRI(ex) + "p"
      val prop = IRI(ex) + "prop"
      val attempt = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
        shape <- schema.shape(S)
      } yield ((shape, schema))
      attempt match {
        case Left(e) => fail(s"Error parsing $e")
        case Right((shape, schema)) => {
          val ip = InversePath(PredicatePath(p))
          shape.propertyShapes.length should be(1)
          val sref = shape.propertyShapes.head
          schema.shapesMap.get(sref) match {
            case Some(ps: PropertyShape) => {
              ps.id should be(prop)
              ps.path should be(ip)
              ps.components should contain only (MinCount(1))
            }
            case other => fail(s"Unexpected value $other")
          }
        }
      }
    }

    it("should be able to get shape with minInclusive") {
      val ex = IRI("http://example.org/")
      val str =
        """|@prefix : <http://example.org/>
         |@prefix sh: <http://www.w3.org/ns/shacl#>
         |
         |:S a sh:Shape;
         |   sh:property :prop .
         |
         |:prop sh:path :p;
         |       sh:minInclusive 3 .
         |""".stripMargin
      val S = ex + "S"
      val prop = ex + "prop"
      val attempt = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
        schema <- RDF2Shacl.getShacl(rdf)
        shape <- schema.shape(S)
        propShape <- schema.shape(prop)
      } yield ((shape, propShape))
      //val p1 = Shape.emptyPropertyShape(prop, PredicatePath(p)).copy(components = Seq(NodeKind(IRIKind)))
      attempt match {
        case Left(e) => fail(s"Failed $e")
        case Right((shape,propShape)) => {
          shape.propertyShapes should contain only (RefNode(prop))
          propShape.components should contain only (MinInclusive(IntegerLiteral(3,"3"))) 
        }
      }

    }

} 

}