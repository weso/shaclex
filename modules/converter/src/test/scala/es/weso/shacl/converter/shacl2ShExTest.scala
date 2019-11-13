package es.weso.shacl.converter

import cats._
import cats.implicits._
import es.weso._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shex.implicits.eqShEx._
import org.scalatest._

class shacl2ShExTest extends FunSpec with Matchers with EitherValues {

  describe("shacl2ShEx converter") {
    {
    shouldConvertSHACLShEx(
      """|prefix : <http://example.org/>
         |prefix sh: <http://www.w3.org/ns/shacl#>
         |:S a sh:NodeShape ;
         |   sh:nodeKind sh:IRI .
         """.stripMargin,
      """|prefix : <http://example.org/>
         |prefix sh: <http://www.w3.org/ns/shacl#>
         |:S IRI
         """.stripMargin)

      shouldConvertSHACLShEx(
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |:S a sh:NodeShape ;
           |   sh:datatype xsd:string .
        """.stripMargin,
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |:S xsd:string
        """.stripMargin)

      shouldConvertSHACLShEx(
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |:S a sh:NodeShape ;
           |   sh:datatype xsd:string ;
           |   sh:nodeKind sh:Literal .
        """.stripMargin,
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |:S xsd:string AND Literal
        """.stripMargin)

      shouldConvertSHACLShEx(
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |:S a sh:NodeShape ;
           |   sh:in ("hi" 2)  ;
        """.stripMargin,
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
           |:S ["hi" 2]
        """.stripMargin)

      shouldConvertSHACLShEx(
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |:PS a sh:PropertyShape ;
           |    sh:path :p ;
           |    sh:nodeKind sh:IRI .
        """.stripMargin,
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |:PS { :p IRI }
        """.stripMargin)


  shouldConvertSHACLShEx(
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |:S a sh:NodeShape ;
           |   sh:property :PS .
           |:PS
           |    sh:path :p ;
           |    sh:nodeKind sh:IRI .
        """.stripMargin,
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |:S { &:PS }
           |
           |_:1 {
           | $:PS :p IRI
           |}
        """.stripMargin)


    }

  }

  def shouldConvertSHACLShEx(strSHACL: String, expected: String): Unit = {
    it(s"Should convert: $strSHACL to ShEx and obtain: $expected") {
    val r = for {
      shaclRDF       <- RDFAsJenaModel.fromChars(strSHACL, "TURTLE", None)
      shacl          <- RDF2Shacl.getShacl(shaclRDF)
      shexConverted  <- Shacl2ShEx.shacl2ShEx(shacl).leftMap(e => s"Error in conversion: $e")
      expectedSchema <- shex.Schema.fromString(expected, "ShExC")
    } yield (shexConverted, expectedSchema, shacl)
    r.fold(
        e => fail(s"Error: $e"),
        values => {
          val (converted, expected, shacl) = values
          val (schema,shapeMap) = converted
          if (Eq[shex.Schema].eqv(schema,expected)) {
            info(s"Schemas are equals")
          } else {
            fail(s"SHACL2ShEx schemas are not equal: SHACL:\n$shacl\n---\nSHACL converted to ShEx:\n${schema}\nExpected:\n$expected")
          }
        }
      )
    }
  }

}
