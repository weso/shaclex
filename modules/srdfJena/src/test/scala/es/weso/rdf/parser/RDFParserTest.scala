package es.weso.rdf.parser
import org.scalatest._
import util._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.parser._

class RDFParserTest extends FunSpec with Matchers with RDFParser with TryValues {

  describe("RDFParser") {

    describe("iriFromPredicate") {
      it("iriFromPredicate simple") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x :p :T .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          val p: IRI = IRI("http://example.org/p")
          obj <- iriFromPredicate(p)(n, rdf)
        } yield (obj)
        try1.success.value should be(IRI("http://example.org/T"))
      }
      it("iriFromPredicate fails when more than one matches") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x :p :T, :S .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          val p: IRI = IRI("http://example.org/p")
          obj <- iriFromPredicate(p)(n, rdf)
        } yield (obj)
        try1.failure.exception.getMessage should include("More than one value from predicate")
      }
      it("iriFromPredicate fails when no predicate") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x :p :T .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          val p: IRI = IRI("http://example.org/q")
          obj <- iriFromPredicate(p)(n, rdf)
        } yield (obj)
        try1.failure.exception.getMessage should include("Not found triples with subject")
      }

    }

    describe("rdfType") {
      it(
        "rdfType simple") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x a :T .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          obj <- rdfType(n, rdf)
        } yield (obj)
        try1.success.value should be(IRI("http://example.org/T"))
      }
      it(
        "rdfType fails when more than one type") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x a :T, :S .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          obj <- rdfType(n, rdf)
        } yield (obj)
        try1.failure.exception.getMessage should include("is not single")
      }
      it(
        "rdfType fails when no type") {
        val cs =
          """|prefix : <http://example.org/>
                  |:x :p :T .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          val p: IRI = IRI("http://example.org/q")
          obj <- rdfType(n, rdf)
        } yield (obj)
        try1.failure.exception.getMessage should include("no type")
      }

    }

    describe(
      "rdfList") {
      it("rdfList happy path") {
        val
        cs = """|prefix : <http://example.org/>
                  |:x :p (1 2 3) .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          val p = IRI("http://example.org/p")
          nodeLs <- objectFromPredicate(p)(n, rdf)
          ls <- rdfList(nodeLs, rdf)
        } yield (ls)
        try1.success.value should be(List(IntegerLiteral(1), IntegerLiteral(2), IntegerLiteral(3)))
      }

      it(
        "rdfList empty") {
        val cs = """|prefix : <http://example.org/>
                  |:x :p () .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          val p = IRI("http://example.org/p")
          nodeLs <- objectFromPredicate(p)(n, rdf)
          ls <- rdfList(nodeLs, rdf)
        } yield (ls)
        try1.success.value should be(List())
      }

    }
    describe("rdfListForPredicate") {
      it(
        "rdfListForPredicate happy path") {
        val cs = """|prefix : <http://example.org/>
                  |:x :p (1 2 3) .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          val p = IRI("http://example.org/p")
          ls <- rdfListForPredicate(p)(n, rdf)
        } yield (ls)
        try1.success.value should be(List(IntegerLiteral(1), IntegerLiteral(2), IntegerLiteral(3)))
      }
    }
    describe("integerLiteralForPredicate") {
      it(
        "integerLiteralForPredicate happy path") {
        val cs = """|prefix : <http://example.org/>
                  |:x :p 1 .""".stripMargin
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          val n: RDFNode = IRI("http://example.org/x")
          val p = IRI("http://example.org/p")
          n <- integerLiteralForPredicate(p)(n, rdf)
        } yield (n)
        try1.
          success.value should be(1)
      }
    }

    describe("anyOf") {
      it("anyOf when some one is ok") {
        val cs = """|prefix : <http://example.org/>
                    |:x :p :y .""".stripMargin
        val y = IRI("http://example.org/y")
        val try1 = for {
          rdf <- RDFAsJenaModel.fromChars(cs, "TURTLE")
          n: RDFNode = IRI("http://example.org/x")
          p = IRI("http://example.org/p")
          q = IRI("http://example.org/q")
          n <- anyOf(objectFromPredicate(p),objectFromPredicate(q))(n, rdf)
        } yield (n)
        try1
        match {
          case Failure(e) => fail(s"Error: $e")
          case Success(value) => value should contain only (y)
        }
    }

    it("anyOf when some all fail should fail") {
      val cs =
        """|prefix : <http://example.org/>
                  |:x :p :y .""".stripMargin
      val y = IRI(
        "http://example.org/y")
      val q = IRI(
        "http://example.org/q")
      val r = IRI("http://example.org/r")
      val try1 = for {
        rdf <-
        RDFAsJenaModel.fromChars(cs, "TURTLE")
        n: RDFNode = IRI(
          "http://example.org/x"
        )
        n <- anyOf(objectFromPredicate(q),objectFromPredicate(r))(n, rdf)
      } yield (n)
      try1 match {
        case Failure(e) => fail(s"Failed with $e")
        case Success(values) => values shouldBe empty
      }
    }
  }
 }
}
