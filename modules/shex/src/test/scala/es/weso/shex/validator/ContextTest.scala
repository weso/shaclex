package es.weso.shex.validator

import org.scalatest._
import cats._

class ContextTest extends FunSpec with Matchers with EitherValues {

  describe(s"Context") {
    it("should generate empty context") {
      val ctx = Monoid[Context].empty
      ctx.typing.getMap.isEmpty should be(true)
    }
  }

}
