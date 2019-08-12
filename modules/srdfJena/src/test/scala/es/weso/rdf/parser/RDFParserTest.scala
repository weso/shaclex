package es.weso.rdf.parser
import org.scalatest._
import util._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._

class RDFParserTest extends FunSpec with Matchers with RDFParser with EitherValues {

  describe("RDFParser") {

    describe("iriFromPredicate") {
      it("iriFromPredicate simple") {
        val cs = """|prefix : <http://example.org/>
                   |:x :p :T .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p: IRI = IRI("http://example.org/p")
          obj <- iriFromPredicate(p)(n, rdf)
        } yield (obj)
        try1.right.value should be(IRI("http://example.org/T"))
      }
      it("iriFromPredicate fails when more than one matches") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x :p :T, :S .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p: IRI = IRI("http://example.org/p")
          obj <- iriFromPredicate(p)(n, rdf)
        } yield (obj)
        try1 match {
          case Left(s) => s should include("More than one value from predicate")
          case Right(v) => fail(s"Parsed as $v when it should fail")
        }
      }
      it("iriFromPredicate fails when no predicate") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x :p :T .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p: IRI = IRI("http://example.org/q")
          obj <- iriFromPredicate(p)(n, rdf)
        } yield (obj)
        try1 match {
          case Left(s) => s should include("Not found triples with subject")
          case Right(v) => fail(s"Parsed as $v when it should fail")
        }
      }

    }

    describe("rdfType") {
      it("rdfType simple") {
        val cs = """|prefix : <http://example.org/>
                  |:x a :T .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          obj <- rdfType(n, rdf)
        } yield (obj)
        try1.right.value should be(IRI("http://example.org/T"))
      }
      it("rdfType fails when more than one type") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x a :T, :S .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          obj <- rdfType(n, rdf)
        } yield (obj)
        try1 match {
          case Left(s) => s should include("More than one value")
          case Right(v) => fail(s"Parsed as $v when it should fail")
        }
      }
      it("rdfType fails when no type") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x :p :T .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p: IRI = IRI("http://example.org/q")
          obj <- rdfType(n, rdf)
        } yield (obj)
        try1 match {
          case Left(s) => s should include("Not found triples")
          case Right(v) => fail(s"Parsed as $v when it should fail")
        }
      }

    }

    describe(
      "rdfList") {
        it("rdfList happy path") {
          val cs = """|prefix : <http://example.org/>
                  |:x :p (1 2 3) .""".stripMargin
          val try1 = for {
            rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
            n: RDFNode = IRI("http://example.org/x")
            p = IRI("http://example.org/p")
            nodeLs <- objectFromPredicate(p)(n, rdf)
            ls <- rdfList(nodeLs, rdf)
          } yield (ls)
          try1.right.value should be(List(IntegerLiteral(1,"1"), IntegerLiteral(2,"2"), IntegerLiteral(3,"3")))
        }

        it("rdfList empty") {
          val cs = """|prefix : <http://example.org/>
                  |:x :p () .""".stripMargin
          val try1 = for {
            rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
            n: RDFNode = IRI("http://example.org/x")
            p = IRI("http://example.org/p")
            nodeLs <- objectFromPredicate(p)(n, rdf)
            ls <- rdfList(nodeLs, rdf)
          } yield (ls)
          try1.right.value should be(List())
        }

      }
    describe("rdfListForPredicate") {
      it("rdfListForPredicate happy path") {
        val cs = """|prefix : <http://example.org/>
                  |:x :p (1 2 3) .
                  |""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p = IRI("http://example.org/p")
          ls <- rdfListForPredicate(p)(n, rdf)
        } yield (ls)
        try1.right.value should be(List(IntegerLiteral(1,"1"), IntegerLiteral(2,"2"), IntegerLiteral(3,"3")))
      }
    }
    describe("integerLiteralForPredicate") {
      it("integerLiteralForPredicate happy path") {
        val cs = """|prefix : <http://example.org/>
                  |:x :p 1 .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p = IRI("http://example.org/p")
          n <- integerLiteralForPredicate(p)(n, rdf)
        } yield (n)
        try1.right.value should be(1)
      }
    }

  /*  describe("anyOf") {
      it("anyOf when some one is ok") {
        val cs = """|prefix : <http://example.org/>
                    |:x :p :y .""".stripMargin
        val y = IRI("http://example.org/y")
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p = IRI("http://example.org/p")
          q = IRI("http://example.org/q")
          n <- anyOf(objectFromPredicate(p), objectFromPredicate(q))(n, rdf)
        } yield (n)
        try1 match {
          case Left(e) => fail(s"Error: $e")
          case Right(value) => value should contain only (y)
        }
      }

      it(
        "anyOf when some all fail should fail") {
          val cs =
            """|prefix : <http://example.org/>
                  |:x :p :y .""".stripMargin
          val q = IRI("http://example.org/q")
          val r = IRI("http://example.org/r")
          val try1 = for {
            rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
            n: RDFNode = IRI(
              "http://example.org/x")
            n <- anyOf(
              objectFromPredicate(q),
              objectFromPredicate(r))(n, rdf)
          } yield (n)
          try1 match {
            case Left(e) => fail(s"Failed with $e")
            case Right(values) => values shouldBe empty
          }
        }
    }

    describe("arc") {
      it("parses single arc") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                   |:x :p () .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
          v <- arc(p, rdfNil)(x, rdf)
        } yield (v)
        try1 match {
          case Left(e) => fail(s"Failed with $e")
          case Right(value) => value shouldBe List()
        }
      }
    }
    describe("list1Plus") {
      it("parses list1Plus ") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |:x :p (:y :z) .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        val y = iriEx + "y"
        val z = iriEx + "z"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
          v <- arc(p, list1Plus(condition(isIri, "isIRI")))(x, rdf)
        } yield (v)
        try1 match {
          case Left(e) => fail(s"Failed with $e")
          case Right(values) => values should contain theSameElementsAs List(y, z)
        }
      }

      it("fails list1Plus with recursive nodes in RDF list") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     |:x :p _:1 .
                     |_:1 rdf:first :y .
                     |_:1 rdf:rest _:1 .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        val y = iriEx + "y"
        val z = iriEx + "z"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
          v <- arc(p, list1Plus(condition(isIri, "isIRI")))(x, rdf)
        } yield (v)
        try1 match {
          case Left(e) => info("fails as expected")
          case Right(values) => fail(s"Fails because it obtained values $values but should have failed")
        }
      }

    }

    describe("list2Plus") {
      it("parses list2Plus ") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |:x :p (:y :z) .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        val y = iriEx + "y"
        val z = iriEx + "z"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
          v <- arc(p, list2Plus(condition(isIri, "isIRI")))(x, rdf)
        } yield (v)
        try1 match {
          case Left(e) => fail(s"Failed with $e")
          case Right(values) => values should contain theSameElementsAs List(y, z)
        }
      }

      it("fails list2Plus if only one") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |:x :p (:y ) .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        val y = iriEx + "y"
        val z = iriEx + "z"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
          v <- arc(p, list2Plus(condition(isIri, "isIRI")))(x, rdf)
        } yield (v)
        try1 match {
          case Left(e) => info(s"Failed as expected $e")
          case Right(values) => fail(s"Should fail if only one value but succedded with $values")
        }
      }

      it("fails list2Plus with recursive nodes in RDF list") {
        val ex = "http://example.org/"
        val iriEx = IRI(ex)
        val str = s"""|prefix : <$ex>
                     |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                     |:x :p _:1 .
                     |_:1 rdf:first :y .
                     |_:1 rdf:rest _:1 .""".stripMargin
        val x = iriEx + "x"
        val p = iriEx + "p"
        val y = iriEx + "y"
        val z = iriEx + "z"
        def isIri(n: RDFNode): Boolean = n.isIRI
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE")
          v <- arc(p, list2Plus(condition(isIri, "isIRI")))(x, rdf)
        } yield (v)
        try1 match {
          case Left(e) => info("fails as expected")
          case Right(values) => fail(s"Fails because it obtained values $values but should have failed")
        }
      }
    } */
  }
}
