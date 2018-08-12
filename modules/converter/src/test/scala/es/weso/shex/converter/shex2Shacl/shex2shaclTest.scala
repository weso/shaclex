package es.weso.shex.converter

import es.weso._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shacl.converter.Shacl2RDF
import org.scalatest._
import cats.implicits._

class shex2shaclTest extends FunSpec with Matchers with EitherValues {

  describe(s"ShEx2SHACL") {
    shouldConvertShacl2Shex(
      """|prefix : <http://example.org/>
        |:S IRI
      """.stripMargin, """prefix : <http://example.org/>
        |prefix sh: <http://www.w3.org/ns/shacl#>
        |:S a sh:NodeShape ;
        |   sh:closed false ;
        |   sh:nodeKind sh:IRI .
      """.stripMargin)
  }

  def shouldConvertShacl2Shex(shexStr: String, shaclStrExpected: String): Unit = {
    it(s"Should convert ShEx->SHACL\n$shexStr\n$shaclStrExpected\n--->") {
      val result: Either[String,Boolean] = for {
        schema <- shex.Schema.fromString(shexStr,"SHEXC",None, RDFAsJenaModel.empty)
        shaclSchema <- ShEx2Shacl.shex2Shacl(schema).toEither.
          leftMap(e => s"Error converting schema: $e\n$schema")
        rdfShacl = (new Shacl2RDF {}).toRDF(shaclSchema, RDFAsJenaModel.empty)
        rdfShaclExpected <- RDFAsJenaModel.fromChars(shaclStrExpected,"TURTLE",None)
        b <-rdfShacl.isIsomorphicWith(rdfShaclExpected)
        _ <- if (!b)
           Left("RDFs are not isomorphic. Obtained RDF:\n" +
             s"${rdfShacl.serialize("TURTLE")}\n" +
             s"Expected: \n${rdfShaclExpected.serialize("TURTLE")}")
          else Right(())
      } yield b
      result.fold(e => fail(s"Error: $e"),
        b => b should be(true))
    }
  }
}
