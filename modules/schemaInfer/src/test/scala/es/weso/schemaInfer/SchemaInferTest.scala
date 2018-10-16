package es.weso.schemaInfer
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.parser.RDFParser
import es.weso.schema.Schemas
import es.weso.shapeMaps.NodeSelector
import org.scalatest.{FunSpec, Matchers}

class SchemaInterTest extends FunSpec with Matchers with RDFParser {

  describe(s"Schema Infer") {
    it(s"Should infer a ShEx schema from basic RDF node") {
      val rdfStr =
        """|prefix : <http://example.org/>
          |:x :p 1, "Hi" ;
          |   :q "Hi" ;
          |   :r :x .
        """.stripMargin
      val expectedStr =
        """|prefix : <http://example.org/>
           |:S {
           | :p Literal ;
           |  :q xsd:string ;
           |  :r IRI
           |}
        """.stripMargin
      val nodeSelectorStr = ":x"
      val result = for {
       rdf <- RDFAsJenaModel.fromChars(rdfStr, "TURTLE")
       schemaExpected <- Schemas.fromString(expectedStr,"ShExC","ShEx")
       nodeSelector <- NodeSelector.fromString(nodeSelectorStr, None,rdf.getPrefixMap)
       schema <- SchemaInfer.infer(rdf, nodeSelector, "ShEx")
      } yield (rdf, schemaExpected, nodeSelector, schema)

    }
  }
}