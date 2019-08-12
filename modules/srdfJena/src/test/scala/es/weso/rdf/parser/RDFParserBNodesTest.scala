package es.weso.rdf.parser

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI}
import org.scalatest._

class RDFParserBNodesTest extends FunSpec with Matchers with RDFParser with EitherValues {

  describe("RDFParser for BNodes") {

      it("check if bnode Ids are maintained") {
        val cs = """|prefix : <http://example.org/>
                   |_:x :p _:y .
                   |_:y :q 1""".stripMargin
        val eitherValue = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
        } yield (rdf)
        eitherValue.fold(e => fail(s"Parse error: $e"), value => {
        val rdf = value
        rdf.triplesWithSubject(BNode("x")).right.value.map(_.obj).headOption.fold(
          fail(s"No triples with subject _:x"))(
          node => {
            node should be(BNode("y"))
            info(s"${rdf.triplesWithSubject(BNode("x"))}")
            info(s"\nStatements with :p:\n ${rdf.triplesWithPredicate(IRI("http://example.org/p"))}")
            info(s"\nParsed:\n${rdf.serialize("N-TRIPLES").getOrElse("")}")
          })})
      }
  }
}
