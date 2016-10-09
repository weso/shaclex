package es.weso.validating

import cats.implicits._
import org.scalatest._
import ConstraintError._

class ConstraintErrorTest extends FunSpec with Matchers with OptionValues {

  describe("ConstraintError") {
    val m : ConstraintError[Int] = MsgError("example")
    val ce : ConstraintError[Int] = All_SomeNotValid("all", Seq(m))
    val cem : ConstraintError[Double]= ce.map(n => n.toDouble)
    cem.msg should be("all")
  }

}
