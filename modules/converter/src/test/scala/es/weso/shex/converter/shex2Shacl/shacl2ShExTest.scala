package es.weso.shex.converter.uml

import cats._
import cats.implicits._
import es.weso._
import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.shacl._
import es.weso.shacl.converter._
import es.weso.shex.converter.ShEx2Shacl
import org.scalatest._

class shacl2ShExTest extends FunSpec with Matchers with EitherValues {

  describe("shacl2ShEx converter") {
    {
      val strSHACL =
        """
          |prefix : <http://example.org/>
          |prefix sh: <http://www.w3.org/ns/shacl#>
          |:S a sh:NodeShape ;
          |   sh:nodeKind sh:IRI .
        """.stripMargin
      val strShEx =
        """
          |prefix : <http://example.org/>
          |prefix sh: <http://www.w3.org/ns/shacl#>
          |:S IRI
        """.stripMargin
    shouldConvertSHACLShEx(strSHACL, strShEx)
    }
  }

  def shouldConvertSHACLShEx(strSHACL: String, expected: String): Unit = {
    it(s"Should convert: $strSHACL to ShEx and obtain: $expected") {
    val emptyRdf: RDFReader = RDFAsJenaModel.empty
    val r = for {
      shaclRDF       <- RDFAsJenaModel.fromChars(strSHACL, "TURTLE", None)
      shacl          <- RDF2Shacl.getShacl(shaclRDF)
      shexConverted  <- Shacl2ShEx.shacl2ShEx(shacl).toEither.leftMap(es => es.toList.mkString(","))
      expectedSchema <- shex.Schema.fromString(expected, "ShExC", None, emptyRdf)
    } yield (shexConverted, expectedSchema, shacl)
    r.fold(
        e => fail(s"Error: $e"),
        values => {
          val (shexConverted, expected, shacl) = values
          info(s"SHACL2ShEx: SHACL\n$shacl\nSHACL converted to ShEx:\n$shexConverted\nExpected:\n$expected")
          shexConverted should be(expected)
        }
      )
    }
  }

}
