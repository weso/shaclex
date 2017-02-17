package es.weso.shex.shexR

import es.weso.rdf.{PrefixMap, RDFReader}
import es.weso.shex._
import org.scalatest._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNodeId, IRI}
import es.weso.shex.shexR.PREFIXES._
import org.apache.jena.rdf.model.Model

import scala.util.{Failure, Success}

class RDF2ShExTest extends FunSpec with Matchers with EitherValues with TryValues {
  val rdf2Shex = new RDF2ShEx {}


  describe("Simple schema") {
    it("should parse simple schema") {
      val str =
        """
        |prefix sx: <http://shex.io/ns/shex#>
        |prefix : <http://example.org/>
        |
        |:schema a sx:Schema ;
        |  sx:shapes [ a sx:NodeConstraint ;
        |              sx:nodeKind sx:iri
        |            ] .
      """.
          stripMargin

      val expected = Schema(
        None,
        None,
        None,
        None,
        Some(Map(BNodeLabel(BNodeId("1")) -> NodeConstraint.nodeKind(IRIKind, List())))
      )

      val result = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", None)
        schemas <- rdf2Shex.getSchemas(rdf)
      } yield schemas

      result match {
        case Success(ss) => ss.size match {
          case 0 => fail("No schema found")
          case 1 => {
            val schema = ss.head
            val model1 = ShEx2RDF.shEx2Model(schema, Some(IRI("http://example.org/x")))
            val model2 = ShEx2RDF.shEx2Model(expected, Some(IRI("http://example.org/x")))
            if (model1.isIsomorphicWith(model2)) {
              info(s"Models are isomorphic")
            } else {
              info(s"Schema obtained: ${model1}\nSchema expected: ${model2} are not isomorphic")
              fail("Schemas are not isomorphic")
            }
          }
          case _ => fail("More than one schema returned")
        }
      case Failure(e) => {
            info(s"Failed $e")
            fail(e)
          }
        }
    }

    describe("opt") {

      it("Should pass opt when option exists") {
        val str =
          """|prefix sx: <http://shex.io/ns/shex#>
         |prefix : <http://example.org/>
         |
         |:x sx:start :S .
         |""".
            stripMargin

        val result = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", None)
          schemas <- rdf2Shex.opt(sx_start, rdf2Shex.iri)(IRI("http://example.org/x"),rdf)
        } yield schemas

        result match {
          case Success(v) => v should be(Some(IRI("http://example.org/S")))
          case Failure(e) => {
            info(s"Failed $e")
            fail(e)
          }
        }
      }

      it("Should pass opt with None when it doesn't exist") {
        val str =
          """|prefix sx: <http://shex.io/ns/shex#>
             |prefix : <http://example.org/>
             |
             |:x a :S .
             |""".
            stripMargin

        val result = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", None)
          schemas <- rdf2Shex.opt(sx_start, rdf2Shex.iri)(IRI("http://example.org/x"),rdf)
        } yield schemas

        result match {
          case Success(v) => v should be(None)
          case Failure(e) => {
            info(s"Failed $e")
            fail(e)
          }
        }
      }
    }

    it("Should parse schema with 2 shape expressions") {
      val ex = "http://example.org/"
      val rdfStr =
        s"""|prefix : <$ex>
            |prefix sx: <http://shex.io/ns/shex#>
            |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
            |:S a sx:Schema ;
            |   sx:shapes [
            |     a sx:Shape ;
            |     sx:expression [
            |       a sx:tripleConstraint;
            |       sx:predicate :p ;
            |       sx:valueExpr [ a sx:NodeConstraint ; sx:datatype xsd:string ]
            |     ]
            |  ] .
         """.stripMargin
      val result = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr, "TURTLE", None)
        schemas <- rdf2Shex.opt(sx_start, rdf2Shex.iri)(IRI("http://example.org/x"),rdf)
      } yield schemas

      result match {
        case Success(v) => v should be(None)
        case Failure(e) => {
          info(s"Failed $e")
          fail(e)
        }
      }
    }
  }




}
