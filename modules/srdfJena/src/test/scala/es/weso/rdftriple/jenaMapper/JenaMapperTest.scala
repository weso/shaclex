package es.weso.rdftriple.jenaMapper
import org.scalatest.FunSpec
import org.apache.jena.rdf.model.ModelFactory
import org.scalatest.Matchers
import es.weso.rdf.nodes._
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdf.jena._
import es.weso.rdf.triples.RDFTriple
import org.apache.jena.query.{ResultSet, ResultSetFactory}
import org.apache.jena.sparql.engine.QueryIterator

class JenaMapperTest
  extends FunSpec
  with JenaBased
  with Matchers {

  describe("Jena Mapper") {
    it("Should compare one triple with 2 different bNodes") {
      val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BNode("b" + 1)))
      val s = """[] <http://example.org#p> [] ."""
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty,None)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with a shared bNode") {
      val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BNode("b" + 0)))
      val s = """_:a <http://example.org#p> _:a ."""
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty,None)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with a prefix decl") {
      val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BNode("b" + 0)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p _:a .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty,None)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with an integer literal") {
      val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), IntegerLiteral(1)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p 1 .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty,None)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with a decimal literal") {
      val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), DecimalLiteral(1.2)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p 1.2 .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty,None)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with a boolean literal") {
      val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BooleanLiteral(true)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p true .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty,None)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    // The following test fails probably for Double comparison
    ignore("Should compare one triple with a double literal") {
      val ts = Set(RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), DoubleLiteral(1.2e3)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p 1.2e3 .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty,None)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should convert three triples") {
      val ts = Set(
        RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), BNode("b" + 0)),
        RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), IntegerLiteral(4)),
        RDFTriple(BNode("b" + 0), IRI("http://example.org#p"), LangLiteral("pepe", Lang("es"))))
      val empty = ModelFactory.createDefaultModel
      val m1 = RDFTriples2Model(ts, empty,None)
      val m2 = str2model("""|@prefix : <http://example.org#> .
                         |_:0 <http://example.org#p> _:0, 4, "pepe"@es .
                         |""".stripMargin)
      shouldBeIsomorphic(m1, m2)
    }

  }

  describe("wellTypedDatatype") {
    it("Should check integer ok") {
      val node = IntegerLiteral(23)
      wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#integer")) match {
        case Left(e) => fail(s"Should be integer: $e")
        case Right(node) => ()
      }
    }
    it("Should check string ok") {
      val node = StringLiteral("Pepe")
      wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#string")) match {
        case Left(e) => fail(s"Should be string: $e")
        case Right(node) => ()
      }
    }
    it("Should check lang string ok") {
      val node = LangLiteral("Pepe", Lang("es"))
      wellTypedDatatype(node, IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")) match {
        case Left(e) => fail(s"Should be lang string: $e")
        case Right(node) => ()
      }
    }
    it("Should check date ok") {
      val node = DatatypeLiteral("1981-07-03", IRI("http://www.w3.org/2001/XMLSchema#date"))
      wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#date")) match {
        case Left(e) => fail(s"Should be date: $e")
        case Right(node) => ()
      }
    }
    it("Should check bad string") {
      val node = DatatypeLiteral("john", IRI("http://www.w3.org/2001/XMLSchema#string"))
      wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#date")) match {
        case Right(false) => info(s"Fails as expected")
        case r => fail(s"Should return false but returned $r")
      }
    }
    it("Should check bad date") {
      val node = DatatypeLiteral("john", IRI("http://www.w3.org/2001/XMLSchema#date"))
      wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#date")) match {
        case Left(e) => info(s"Fails as expected: $e")
        case Right(node) => fail(s"Should fail but passed $node")
      }
    }

    it("Should fail checking single string with dateTime") {
      val node = StringLiteral("not_a_date")
      wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#dateTime")) match {
        case Right(false) => info(s"Fails as expected")
        case r => fail(s"Should return false but returned $r")
      }
    }

    it("Should fail checking date with dateTime") {
      val node = DatatypeLiteral("2017-05-15", IRI("http://www.w3.org/2001/XMLSchema#date"))
      wellTypedDatatype(node, IRI("http://www.w3.org/2001/XMLSchema#dateTime")) match {
        case Right(false) => info(s"Fails as expected")
        case r => fail(s"Should return false but returned $r")
      }
    }

  }

}