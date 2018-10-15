package es.weso.shex.validator

import es.weso.rdf.jena._
import es.weso.rdf.nodes._
import es.weso.shapeMaps.ShapeMap
import es.weso.shex._
import org.scalatest._

class ExprTest extends FunSpec with Matchers with EitherValues {

  describe(s"Parsing exprs") {
    ignore("should parse a simple as") {
      val strSchema =
        """
          |prefix : <http://example.org/>
          |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
          |
          |:R {
          | :a xsd:integer as ?a
          |}
        """.stripMargin

      val eitherResult = for {
        schema <- Schema.fromString(strSchema, "ShExC", None, RDFAsJenaModel.empty)
      } yield schema

      eitherResult.fold(e => fail(s"Error: $e"), r => info(s"Parsed as $r"))
    }
  }

  ignore("Expr test") {
    it(s"Should validate triple with expr") {
      val strSchema =
        """prefix : <http://example.org/>
          |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
          |
          |:R {
          | :a  xsd:integer as $a;
          | :a1 xsd:integer as $a1 ;
          | $a1 = $a + 1
          |}
        """.stripMargin

      var strRdf =
        """
          |prefix : <http://example.org/>
          |
          |:good :a 1 ; :a1 2 .
          |:bad  :a 1 ; :a1 3 .
        """.stripMargin

      val strShapeMap =
        """
          |:good@:R,:bad@:R
        """.stripMargin

      // TODO: Change value of bad to !:R
      val strExpectedShapeMap =
        """
          |:good@:R,:bad@:R
        """.stripMargin

      val eitherResult = for {
        rdf <- RDFAsJenaModel.fromChars(strRdf,"TURTLE",None)
        schema <- Schema.fromString(strSchema,"ShExC",None, RDFAsJenaModel.empty)
        shapeMap <- ShapeMap.fromString(strShapeMap,"Compact",None,rdf.getPrefixMap,schema.prefixMap)
        fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap,rdf,rdf.getPrefixMap,schema.prefixMap)
        expectedShapeMap <- ShapeMap.fromString(strExpectedShapeMap,"Compact", None, rdf.getPrefixMap,schema.prefixMap)
        result <- Validator.validate(schema,fixedShapeMap,rdf)
        expectedShapeMap <- ShapeMap.parseResultMap(strExpectedShapeMap, None, rdf, schema.prefixMap)
        compare <- expectedShapeMap.compareWith(result)
      } yield result

      eitherResult.fold(
        e => fail(s"Error: $e"),
        r => info(s"Result: $r")
      )
    }
  }
}
