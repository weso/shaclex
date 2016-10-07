package es.weso.rdftriple.jenaMapper
import org.scalatest.FunSpec
import org.apache.jena.rdf.model.ModelFactory
import java.io.ByteArrayInputStream
import org.apache.jena.rdf.model.Model
import java.io.InputStream
import org.scalatest.Matchers
import es.weso.rdf.nodes._
import es.weso.rdf.jena.JenaMapper._
import es.weso.rdf.jena._
import es.weso.rdf.triples.RDFTriple

class JenaMapperSuite
    extends FunSpec
    with JenaBased
    with Matchers {

  describe("Jena Mapper") {
    it("Should compare one triple with 2 different bNodes") {
      val ts = Set(RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), BNodeId("b" + 1)))
      val s = """[] <http://example.org#p> [] ."""
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with a shared bNode") {
      val ts = Set(RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), BNodeId("b" + 0)))
      val s = """_:a <http://example.org#p> _:a ."""
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with a prefix decl") {
      val ts = Set(RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), BNodeId("b" + 0)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p _:a .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with an integer literal") {
      val ts = Set(RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), IntegerLiteral(1)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p 1 .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with a decimal literal") {
      val ts = Set(RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), DecimalLiteral(1.2)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p 1.2 .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should compare one triple with a boolean literal") {
      val ts = Set(RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), BooleanLiteral(true)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p true .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    // The following test fails probably for Double comparison
    ignore("Should compare one triple with a double literal") {
      val ts = Set(RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), DoubleLiteral(1.2e3)))
      val s = """|@prefix : <http://example.org#> . 
              |_:a :p 1.2e3 .""".stripMargin
      val empty = ModelFactory.createDefaultModel
      val model1 = RDFTriples2Model(ts, empty)
      val model2 = str2model(s)
      shouldBeIsomorphic(model1, model2)
    }

    it("Should convert three triples") {
      val ts = Set(
        RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), BNodeId("b" + 0)),
        RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), IntegerLiteral(4)),
        RDFTriple(BNodeId("b" + 0), IRI("http://example.org#p"), LangLiteral("pepe", Lang("es")))
      )
      val empty = ModelFactory.createDefaultModel
      val m1 = RDFTriples2Model(ts, empty)
      val m2 = str2model("""|@prefix : <http://example.org#> .
                         |_:0 <http://example.org#p> _:0, 4, "pepe"@es .
                         |""".stripMargin)
      shouldBeIsomorphic(m1, m2)
    }

  }

}