package es.weso.schema

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{ IRI, RDFNode }
import org.scalatest._
import cats.data.EitherT
import cats.effect._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
// import util._

class SchemaTest extends AnyFunSpec with Matchers with EitherValues {

  describe("Simple schema") {
    it("Validates a simple Schema using ShEx") {
      val schema =
        """|prefix : <http://example.org/>
           |:S { :p . }
           |""".stripMargin
      val data =
        """|prefix :   <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |:x :p 1 .
           |:S sh:targetNode :x .
           |""".stripMargin
      val schemaFormat = "SHEXC"
      val dataFormat = "TURTLE"
      val triggerMode = "TARGETDECLS"
      val schemaEngine = "SHEX"
      val node: RDFNode = IRI("http://example.org/x")
      val shape: SchemaLabel = SchemaLabel(IRI("http://example.org/S"))
      val tryResult: EitherT[IO, String, Result] = for {
        schema <- Schemas.fromString(schema, schemaFormat, schemaEngine, None)
        rdf <- RDFAsJenaModel.fromStringIO(data, dataFormat)
      } yield schema.validate(rdf, triggerMode, "", None, None, schema.pm)
      tryResult.value.unsafeRunSync match {
        case Right(result) => {
          info(s"Result: ${result.serialize(Result.TEXT)}")
          info(s"Result solution: ${result.solution}")
          result.isValid should be(true)
          result.hasShapes(node) should contain only (shape)
        }
        case Left(e) => fail(s"Error trying to validate: $e")
      }
    }

    it("fails to validate a wrong SHACL validation") {
      val data =
        """
          |@prefix :      <http://example.org/> .
          |@prefix sh:    <http://www.w3.org/ns/shacl#> .
          |@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .
          |
          |:User   a            sh:NodeShape , rdfs:Class ;
          |        sh:nodeKind  sh:BlankNode .
          |:alice  a       :User .
        """.stripMargin
      val eitherResult = for {
        schema <- Schemas.fromString(data,"TURTLE","SHACLEX",None)
        rdf <- RDFAsJenaModel.fromStringIO(data,"TURTLE",None)
      } yield schema.validate(rdf,"TargetDecls","",None,None,rdf.getPrefixMap,schema.pm)
      eitherResult.value.unsafeRunSync.fold(e => fail(s"Error: $e"), result => {
        result.isValid should be(false)
      })
    }
  }
}
