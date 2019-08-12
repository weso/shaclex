package es.weso.rdf.jena

import org.scalatest.Matchers
import org.scalatest.FunSpec
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes._
import es.weso.rdf._

class RDFAsJenaModelTest
  extends FunSpec
  with JenaBased
  with Matchers {

  describe("Checking base") {
    // println(s"ShaclFolder file...${shaclFolderURI}")

    it("should be able to parse RDF with relative URIs and base") {
      //val emptyModel = ModelFactory.createDefaultModel
      // val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      val map: Map[Prefix, IRI] = Map(Prefix("") -> IRI("http://example.org#"))
      val pm: PrefixMap = PrefixMap(map)
      val str =
        """|@prefix : <http://example.org#> .
                   |:a :b <c> .
                   |""".stripMargin
      val r = for {
       expected <- RDFAsJenaModel.empty.addPrefixMap(pm).
         addBase(IRI("http://example.org/base/")).
         addTriples(Set(RDFTriple(
           IRI("http://example.org#a"),
           IRI("http://example.org#b"),
           IRI("c")))
         )
       rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", Some(IRI("http://example.org/base/")))
      } yield (expected,rdf)
      r.fold(e => fail(s"Error: $e"),
        values => {
          val (expected,rdf) = values
          shouldBeIsomorphic(expected.model, rdf.model)
        })
    }

/*    it("should be able to parse RDF with relative URIs") {
      val emptyModel = ModelFactory.createDefaultModel
      val rdf: RDFAsJenaModel = RDFAsJenaModel(emptyModel)
      rdf.addTriples(Set(RDFTriple(
        IRI("a"),
        IRI("b"),
        IntegerLiteral(1))
      ))
      val str =
        """|<a> <b> 1 .
                   |""".stripMargin
      val m = ModelFactory.createDefaultModel
      RDFAsJenaModel.fromChars(str, "TURTLE", None) match {
        case Right(m2) => shouldBeIsomorphic(rdf.model, m2.model)
        case Left(e) => fail(s"Error $e\n$str")
      }
    } */
  }
}

