package es.weso.rdf.jena

import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import org.scalatest.FunSpec
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._
import es.weso.rdf.jena._
import org.apache.jena.rdf.model.ModelFactory
import es.weso.rdf._
import es.weso.rdf.PREFIXES._
import util._

class RDFAsJenaModelTest
    extends FunSpec
    with JenaBased
    with Matchers {
  describe("Checking base") {
    it("should be able to parse RDF with relative URIs and base") {
      val emptyModel = ModelFactory.createDefaultModel
      val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      val map: Map[String, IRI] = Map("" -> IRI("http://example.org#"))
      val pm: PrefixMap = PrefixMap(map)
      rdf.addPrefixMap(pm)
      rdf.addTriples(Set(RDFTriple(IRI("http://example.org#a"),
                                   IRI("http://example.org#b"),
                                   IRI("http://example.org#c"))))

      val str = """|@prefix : <http://example.org#> .
                   |:a :b <c> .
                   |""".stripMargin
      RDFAsJenaModel.fromChars(str,"TURTLE",Some("http://hola.org/")) match {
        case Success(m2) => shouldBeIsomorphic(rdf.model, m2.model)
        case Failure(e) => fail(s"Error $e\n$str")
      }

    }
}

}
