package es.weso.utils

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI}
import es.weso.rdf.triples.RDFTriple
import org.scalatest._
import es.weso.utils.NormalizeBNodes._

class NormalizeBNodesTest extends FunSpec with Matchers with EitherValues {
  describe(s"Parse RDF with blank nodes") {
    it(s"Should show RDF") {
      val str =
        """|prefix : <http://e.com/>
           |:x :p _:b1 .
           |_:b1 :q :x .
           |_:b1 :r :y .
           |_:b1 :t _:b2 .
           |_:b2 :u _:b1 .
        """.stripMargin

      def iri(x: String) = IRI(s"http://e.com/" + x)
      val rdf1 = RDFAsJenaModel.fromChars(str,"TURTLE",None).right.value
      val n1 = normalizeBNodes(rdf1, RDFAsJenaModel.empty)

      val rdf2 = RDFAsJenaModel.fromChars(str,"TURTLE", None).right.value
      val n2 = normalizeBNodes(rdf2,RDFAsJenaModel.empty)
      val ss1 = n1.triplesWithSubject(BNode("0")).right.value
      val ss2 = n2.triplesWithSubject(BNode("0")).right.value
      val expected = List(
          RDFTriple(BNode("0"), iri("r"), iri("y")),
          RDFTriple(BNode("0"), iri("q"), iri("x")),
          RDFTriple(BNode("0"), iri("t"), BNode("1"))
      )
      ss1 should contain theSameElementsAs expected
      ss2 should contain theSameElementsAs expected
    }
  }


}