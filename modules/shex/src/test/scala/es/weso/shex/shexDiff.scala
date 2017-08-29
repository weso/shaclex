package es.weso.shex
import org.scalatest._
import cats._
import cats.data._
import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.rdf._

class shexDiffTest extends FunSpec with Matchers with EitherValues {

  describe("shexDiff") {
    it("should calculate diffs of base values") {
      val iri = IRI("http://example.org/")
      val x1 = Schema.empty.copy(base = Some(iri))
      val x2 = Schema.empty.copy(base = Some(iri))
      val d = ShExDiff.schemaDiff(x1, x2)
      shouldBeOK(d)
    }
    it("should calculate diffs of start actions") {
      val iri = IRI("http://example.org/")
      val x1 = Schema.empty.copy(startActs = Some(List(SemAct(iri, Some("code")))))
      val x2 = Schema.empty.copy(startActs = Some(List(SemAct(iri, Some("code")))))
      val d = ShExDiff.schemaDiff(x1, x2)
      shouldBeOK(d)
    }
    it("should calculate diffs of prefixes") {
      val iri = IRI("http://example.org/")
      val x1 = Schema.empty.copy(
        prefixes = Some(
          PrefixMap(Map(Prefix(":") -> iri))))
      val x2 = Schema.empty.copy(
        prefixes =
          Some(
            PrefixMap(
              Map(Prefix(":") -> iri))))
      val d = ShExDiff.schemaDiff(x1, x2)
      shouldBeOK(d)
    }
  }

  def shouldBeOK[A](x: ShExDiff.Result[A]): Unit = {
    if (!x.isValid) {
      fail(s"Different: $x")
    }
  }
}
