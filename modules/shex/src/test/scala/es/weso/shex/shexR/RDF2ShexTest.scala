package es.weso.shex.shexR

import es.weso.shex._
import org.scalatest._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.shex.shexR.PREFIXES._
import es.weso.rdf.PREFIXES._


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

      val expected = Schema(IRI(""),
        None,
        None,
        None,
        None,
        Some(List(NodeConstraint.nodeKind(IRIKind, List()))),
        None,
        List())

      val result = for {
        rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", None)
        schemas <- RDF2ShEx.rdf2Schema(rdf)
      } yield schemas

      result match {
        case Right(schema) => {
          val builder = RDFAsJenaModel.empty
          val rdf1 = ShEx2RDF(schema, Some(IRI("http://example.org/x")),builder)
          val rdf2 = ShEx2RDF(expected, Some(IRI("http://example.org/x")),builder)
          if (rdf1.isIsomorphicWith(rdf2).getOrElse(false)) {
            info(s"Models are isomorphic")
          } else {
            info(s"Schema obtained: ${rdf1.serialize("TURTLE")}\nSchema expected: ${rdf2.serialize("TURTLE")} are not isomorphic")
            fail("Schemas are not isomorphic")
          }
        }
        case Left(e) => {
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
          schemas <- rdf2Shex.opt(sx_start, rdf2Shex.iri)(IRI("http://example.org/x"), rdf)
        } yield schemas

        result match {
          case Right(v) => v should be(Some(IRI("http://example.org/S")))
          case Left(e) => {
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
          schemas <- rdf2Shex.opt(sx_start, rdf2Shex.iri)(IRI("http://example.org/x"), rdf)
        } yield schemas

        result match {
          case Right(v) => v should be(None)
          case Left(e) => {
            info(s"Failed $e")
            fail(e)
          }
        }
      }
    }

    it("Should parse schema with a triple constraint") {
      val ex = "http://example.org/"
      val p = IRI(ex) + "p"
      val user = IRILabel(IRI(ex) + "User")
      val expr = IRILabel(IRI(ex) + "expr")
      val v = IRILabel(IRI(ex) + "v")
      val rdfStr =
        s"""|prefix : <$ex>
            |prefix sx: <http://shex.io/ns/shex#>
            |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
            |:S a sx:Schema ;
            |   sx:shapes :User .
            |:User a sx:Shape ;
            |     sx:expression :expr .
            |:expr  a sx:TripleConstraint;
            |       sx:predicate :p ;
            |       sx:valueExpr :v .
            |:v  a sx:NodeConstraint ;
            |    sx:datatype xsd:string .
         """.stripMargin
      val result = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr, "TURTLE", None)
        schema <- RDF2ShEx.rdf2Schema(rdf)
      } yield schema

      val nc = NodeConstraint.datatype(`xsd:string`, List()).addId(v)
      val tc = TripleConstraint.valueExpr(p, nc).addId(expr)
      val shape = Shape.expr(tc).addId(user)

      result match {
        case Right(schema) => schema.shapes should be(Some(List(shape)))
        case Left(e) => {
          info(s"Failed $e")
          fail(e)
        }
      }
    }

    it("Should parse schema with a shapeAnd") {
      val ex = "http://example.org/"
      val p = IRI(ex) + "p"
      val user = IRILabel(IRI(ex) + "User")
      val expr1 = IRILabel(IRI(ex) + "expr1")
      val expr2 = IRILabel(IRI(ex) + "expr2")
      val tc = IRILabel(IRI(ex) + "tc")
      val v = IRILabel(IRI(ex) + "v")
      val rdfStr =
        s"""|prefix : <$ex>
            |prefix sx: <http://shex.io/ns/shex#>
            |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
            |:S a sx:Schema ;
            |   sx:shapes :User .
            |:User a sx:ShapeAnd ;
            |     sx:shapeExprs (:expr1 :expr2) .
            |
            |:expr1 a sx:Shape ;
            |       sx:expression :tc .
            |:tc a  sx:TripleConstraint;
            |       sx:predicate :p ;
            |       sx:valueExpr :v .
            |:v  a sx:NodeConstraint ;
            |    sx:datatype xsd:string .
            |:expr2 a sx:NodeConstraint ;
            |       sx:nodeKind sx:iri .
         """.stripMargin
      val result = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr, "TURTLE", None)
        schema <- RDF2ShEx.rdf2Schema(rdf)
      } yield schema

      val nc = NodeConstraint.datatype(`xsd:string`, List()).addId(v)
      val te: TripleExpr = TripleConstraint.valueExpr(p, nc).addId(tc)
      val se1: ShapeExpr = Shape.expr(te).addId(expr1)
      val se2: ShapeExpr = NodeConstraint.nodeKind(IRIKind, List()).addId(expr2)
      val shape = ShapeAnd(Some(user), List(se1, se2),None,None)

      result match {
        case Right(schema) => schema.shapes should be(Some(List(shape)))
        case Left(e) => {
          info(s"Failed $e")
          fail(e)
        }
      }
    }

  }

}
