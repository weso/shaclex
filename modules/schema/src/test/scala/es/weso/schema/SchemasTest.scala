package es.weso.schema

import es.weso.rdf.jena.RDFAsJenaModel
import org.scalatest._
import matchers.should._
import funspec._
import cats.effect._

class SchemasTest extends AnyFunSpec with Matchers with EitherValues {

  describe("Obtaining schema") {
    it("Obtains Schema from ShEx") {
      val rdfStr = s"""|prefix : <http://example.org>
                       |prefix sh: <http://www.w3.org/ns/shacl#>
                       |:S a sh:NodeShape .""".stripMargin

      val r: IO[(Schema, Either[Throwable,Schema])] = 
       RDFAsJenaModel.fromString(rdfStr,"TURTLE",None).use(rdf => for {
        schema1 <- Schemas.fromRDFIO(rdf,"SHACLex")
        eitherSchema <- ShaclexSchema.empty.fromRDF(rdf).attempt
       } yield (schema1,eitherSchema))
      
      val pair = r.unsafeRunSync
      val (schema,eitherSchema) = pair
      eitherSchema.fold(s => fail(s"Error: $s"), schema2 => schema should be(schema2))
    }
  }
}
