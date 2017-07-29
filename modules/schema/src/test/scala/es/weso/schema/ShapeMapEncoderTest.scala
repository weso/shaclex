package es.weso.schema

import org.scalatest.{EitherValues, FunSpec, Matchers}

class ShapeMapTest extends FunSpec with Matchers with EitherValues {

  describe("ShapeMapTest") {
    it("Parse simple shapeMap") {
      val str="<a>@<x>,<a>@<y>,<b>@<z>"
      val expected = Map(
        "<a>" -> List("<x>","<y>"),
        "<b>" -> List("<z>")
      )
      ShapeMapEncoder.parseShapeMap(Some(str)) should (be(expected))
    }
    it("Parse simple shapeMap with some empty value") {
      val str="<a>@<x>,<a>@<y>,<b>@<z>,@"
      val expected = Map(
        "<a>" -> List("<x>","<y>"),
        "<b>" -> List("<z>"),
        "" -> List("")
      )
      ShapeMapEncoder.parseShapeMap(Some(str)) should (be(expected))
    }
  }
}