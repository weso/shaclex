package es.weso.shacl

import org.scalatest._
import es.weso.typing._

class TypingTest extends FunSpec with Matchers with TryValues with OptionValues {

  describe("Typing") {

    it("should be able to add evidences") {
      type Evidence = String
      type Error = String
      type Node = String
      type Shape = String

      val t1: Typing[Node, Shape, Evidence, Error] = Typing.empty
      val r = t1.addEvidence("x", "S", "x-S1").
        addEvidence("x", "S", "x-S2").
        addEvidence("x", "T", "x-T1").
        addEvidence("x", "T", "x-T2").
        addEvidence("y", "S", "y-S1")
      val oksX = r.getOkValues("x")
      oksX should contain only ("S", "T")
      val es = r.getEvidences("x", "S")
      es.value should contain only ("x-S1", "x-S2")
    }

  }

}
