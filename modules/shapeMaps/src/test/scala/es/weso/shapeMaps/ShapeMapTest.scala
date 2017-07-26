package es.weso.shapeMaps

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._

class ShapeMapTest extends FunSpec with Matchers with TryValues with OptionValues {
  
describe("ShapeMaps") {

  it("should be able to create a shape map") {
    val map = ShapeMap(associations = List(
      Association(nodeSelector = RDFNodeSelector(IRI("http://example.org/x")),shapeLabel=Start)
     )
    )
  }
 }

  describe("ShapeMaps parser") {

    it("should be able to parse a simple shape map") {
      val str = "<http://example.org/x> @ Start"
      val expected = ShapeMap(associations = List(Association(nodeSelector = RDFNodeSelector(IRI("http://example.org/x")),shapeLabel=Start)))
      Parser.parseSchema(str) match {
        case Left(msg) => fail(s"Failed to parse $str: $msg")
        case Right(shapeMap) => shapeMap should be(expected)
      }
    }
  }
}
