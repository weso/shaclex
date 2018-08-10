package es.weso.schema

import org.scalatest._
import cats._
import cats.implicits._

class SchemaConversionsTest extends FunSpec with Matchers with EitherValues {

  ignore("ShExC -> ShExJ") {
    it("Converts simple ShExC to ShExJ") {
      val strShExC =
        """
          |prefix : <http://example.org/>
          |:S { :p IRI }
        """.stripMargin

      val strShExJ =
        """
          | {
          |   "type" : "Schema",
          |   "@context" : "http://www.w3.org/ns/shex.jsonld",
          |   "shapes" : [
          |     {
          |       "type" : "Shape",
          |       "id" : "http://example.org/S",
          |       "expression" : {
          |         "type" : "TripleConstraint",
          |         "predicate" : "http://example.org/p",
          |         "valueExpr" : {
          |           "type" : "NodeConstraint",
          |           "nodeKind" : "iri"
          |         }
          |       }
          |     }
          |   ]
          |}
        """.stripMargin

      val r = for {
        schemaFromShExC <- ShExSchema.fromString(strShExC,"ShExC",None).leftMap(e => s"Error reading ShExC\n$e")
        schemaFromShExJ <- ShExSchema.fromString(strShExJ,"ShExJ",None).leftMap(e => s"Error reading ShExJ\n$e")
        schemaFromConversion <- schemaFromShExC.convert(Some("ShEx"), Some("ShExJ")).leftMap(e => s"Error converting\n$e")
      } yield (schemaFromShExC,schemaFromConversion)

      r.fold(e => fail(s"Error: $e"),
        v => {
          val (s1,s2) = v
          if (s1 == s2) {
            info(s"Schema == schema from conversion\nFrom ShExC: $s1\nFrom ShExJ:$s2")
          } else {
            fail(s"Schema != schema from conversion\nFrom ShExC: $s1\nFrom ShExJ:$s2")
          }
         }
      )
    }

  }
}
