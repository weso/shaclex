package es.weso.shacl.converter

import cats._
import cats.implicits._
import es.weso._
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
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

/*    ignore by now:
  shouldConvertSHACLShEx(
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |:S a sh:NodeShape ;
           |   sh:property [
           |    sh:path :p ;
           |    sh:nodeKind sh:IRI
           |   ] .
        """.stripMargin,
        """|prefix : <http://example.org/>
           |prefix sh: <http://www.w3.org/ns/shacl#>
           |:S {
           | :p IRI
           |}
        """.stripMargin)
    } */

  }

  def shouldConvertSHACLShEx(strSHACL: String, expected: String): Unit = {
    import es.weso.shex.implicits.showShEx._
    it(s"Should convert: $strSHACL to ShEx and obtain: $expected") {
    val emptyRdf: RDFReader = RDFAsJenaModel.empty
    val r = for {
      shaclRDF       <- RDFAsJenaModel.fromChars(strSHACL, "TURTLE", None)
      shacl          <- RDF2Shacl.getShacl(shaclRDF)
      shexConverted  <- Shacl2ShEx.shacl2ShEx(shacl).leftMap(e => s"Error in conversion: $e")
      expectedSchema <- shex.Schema.fromString(expected, "ShExC", None, emptyRdf)
    } yield (shexConverted, expectedSchema, shacl)
    r.fold(
        e => fail(s"Error: $e"),
        values => {
          val (shexConverted, expected, shacl) = values
          info(s"SHACL2ShEx: SHACL\n$shacl\nSHACL converted to ShEx:\n${shexConverted.show}\nExpected:\n$expected")
          shexConverted should be(expected)
        }
      )
    }
  }

}
