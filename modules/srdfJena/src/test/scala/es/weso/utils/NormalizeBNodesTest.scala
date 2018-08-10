package es.weso.utils

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI}
import es.weso.rdf.triples.RDFTriple
import org.scalatest._
import es.weso.utils.NormalizaBNodes._

class NormalizeBNodesTest extends FunSpec with Matchers {
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

      val r = for {
        rdf1 <- RDFAsJenaModel.fromChars(str,"TURTLE",None)
        s1 <- rdf1.serialize("N-TRIPLES")
        n1 = normalizeBNodes(rdf1, RDFAsJenaModel.empty)
        rdf3 <- RDFAsJenaModel.fromChars(str,"TURTLE", None)
        n2 = normalizeBNodes(rdf3,RDFAsJenaModel.empty)
      } yield (n1,n2)
      r.fold(e=> fail(s"Error: $e"), v => {
        val (rdf1,rdf2) = v
        val ss1 = rdf1.triplesWithSubject(BNode("0"))
        val ss2 = rdf2.triplesWithSubject(BNode("0"))
        val expected = List(
          RDFTriple(BNode("0"), iri("r"), iri("y")),
          RDFTriple(BNode("0"), iri("q"), iri("x")),
          RDFTriple(BNode("0"), iri("t"), BNode("1"))
        )
        ss1 should contain theSameElementsAs expected
        ss2 should contain theSameElementsAs expected
      })
    }
  }


}