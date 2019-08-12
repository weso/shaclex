package es.weso.rdf.jena

import es.weso.rdf.nodes._
import es.weso.rdf.path._
import org.scalatest._

class SPARQLQueriesTest
  extends FunSpec
  with JenaBased
  with Matchers {

  describe("SPARQL queries test") {
    it("Should create SPARQL query from a SHACLPath") {
      val sp = PredicatePath(IRI("http://example.org/p"))
      val query = SPARQLQueries.queryPath(sp)
      info(s"Query: $query")
    }

    it("Should create SPARQL query from a inverse SHACLPath") {
      val sp = InversePath(PredicatePath(IRI("http://example.org/p")))
      val query = SPARQLQueries.queryPath(sp)
      info(s"Query: $query")
    }

    it("Should create SPARQL query from a sequence SHACLPath") {
      val sp = SequencePath(Seq(
        PredicatePath(IRI("http://example.org/p")),
        PredicatePath(IRI("http://example.org/q")))
      )
      val query = SPARQLQueries.queryPath(sp)
      info(s"Query: $query")
    }
 }

}