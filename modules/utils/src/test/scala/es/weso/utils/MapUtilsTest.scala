package es.weso.utils

import es.weso.utils.MapUtils._
import org.scalatest._

class MapUtilsTest extends FunSpec with Matchers {

  describe("MapUtils mapMap") {
    it(s"Should convert map of map") {
      val mm: Map[String,Map[String,Int]] = Map(
        "foo" -> Map("a" -> 2, "b" -> 3),
        "bar" -> Map("c" -> 3)
      )
      val mm1: Map[String,Map[String,Int]] = Map(
        "foo1" -> Map("a1" -> 2, "b1" -> 3),
        "bar1" -> Map("c1" -> 3)
      )
      def add1(x: String): String = x ++ "1"
      val r: Map[String,Map[String,Int]] = cnvMapMap(mm, add1, add1, identity[Int])
      r should be(mm1)
    }
  }


}