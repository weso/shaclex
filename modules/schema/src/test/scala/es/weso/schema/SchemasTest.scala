package es.weso.schema

import es.weso.rdf.jena.RDFAsJenaModel
import cats.effect._
import munit._

class SchemasTest extends CatsEffectSuite {

  test("Obtains Schema from ShEx") {
      val rdfStr = s"""|prefix : <http://example.org>
                       |prefix sh: <http://www.w3.org/ns/shacl#>
                       |:S a sh:NodeShape .""".stripMargin

      val r: IO[(Schema, Schema)] = 
       RDFAsJenaModel.fromString(rdfStr,"TURTLE",None).flatMap(_.use(rdf => for {
        schema1 <- Schemas.fromRDFIO(rdf,"SHACLex")
        schema2 <- ShaclexSchema.empty.fromRDF(rdf)
       } yield (schema1,schema2)))
      
      r.map { case (schema1, schema2) => assertEquals(schema1,schema2) } 
    }
}

