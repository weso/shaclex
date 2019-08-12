package es.weso.typing

import org.scalatest._
import cats._
import cats.implicits._
import TypingResult._

class TypingResultTest extends FunSpec with Matchers {

  case class Er(s: String)
  case class Ev(e: String)

  describe("TypingResult") {
    it("get evidences of empty") {
      val m: TypingResult[Er, Ev] = Monoid[TypingResult[Er,Ev]].empty
      m.getEvidences should be (Some(List()))
    }
    it("add simple evidence") {
      val m: TypingResult[Er, Ev] = Monoid[TypingResult[Er,Ev]].empty
      val r = m.addEvidence(Ev("e1"))
      r.getEvidences should be (Some(List(Ev("e1"))))
    }
    it("add two evidences") {
      val m: TypingResult[Er, Ev] = Monoid[TypingResult[Er,Ev]].empty
      val r = m.addEvidence(Ev("e1")).addEvidence(Ev("e2"))
      r.getEvidences should be (Some(List(Ev("e1"),Ev("e2"))))
    }
    it("add no evidence to two evidences") {
      val m: TypingResult[Er, Ev] = Monoid[TypingResult[Er,Ev]].empty
      val r = m.addEvidence(Ev("e1")).addEvidence(Ev("e2")).addNotEvidence(Er("er1"))
      r.getEvidences should be (None)
      r.getErrors should be (Some(List(Er("er1"))))
    }
    it("can combine two valid results ") {
      val m1 = Monoid[TypingResult[Er,Ev]].empty.addEvidence(Ev("e1")).addEvidence(Ev("e2"))
      val m2 = Monoid[TypingResult[Er,Ev]].empty.addEvidence(Ev("e2")).addEvidence(Ev("e3"))
      val r = m1.combine(m2)
      r.getErrors should be (None)
      r.getEvidences.fold(fail(s"No value for $r evidences"))(ls => ls should contain theSameElementsAs(List(Ev("e1"),Ev("e2"),Ev("e2"),Ev("e3"))))
    }
    it("can combine one valid result and one failed result") {
      val m1 = Monoid[TypingResult[Er,Ev]].empty.addEvidence(Ev("e1")).addEvidence(Ev("e2"))
      val m2 = Monoid[TypingResult[Er,Ev]].empty.addEvidence(Ev("e2")).addNotEvidence(Er("er3"))
      val r = m1.combine(m2)
      r.getErrors.fold(fail(s"No value for $r"))(ls => ls should contain theSameElementsAs (List(Er("er3"))))
      r.getEvidences should be (None)

      val r1 = m2.combine(m1)
      r1.getErrors.fold(fail(s"No value for $r1"))(ls => ls should contain theSameElementsAs (List(Er("er3"))))
      r1.getEvidences should be (None)
    }

  }

}
