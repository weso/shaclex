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
    val nodesPrefixMap = PrefixMap.empty.
      addPrefix("",IRI("http://default.org/")).
      addPrefix("ex",IRI("http://example.org/"))
    val shapesPrefixMap = PrefixMap.empty.
      addPrefix("",IRI("http://default.shapes.org/")).
      addPrefix("ex",IRI("http://shapes.org/"))
    val rdfType = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")

    shouldParse("<http://example.org/x> @ Start",
      ShapeMap(List(Association(nodeSelector = RDFNodeSelector(IRI("http://example.org/x")),shapeLabel=Start))),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse("<http://example.org/x>@Start",
      ShapeMap(List(Association(nodeSelector = RDFNodeSelector(IRI("http://example.org/x")),shapeLabel=Start))),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse("<http://example.org/x>@<http://example.org/S>",
      ShapeMap(List(Association(nodeSelector = RDFNodeSelector(IRI("http://example.org/x")),shapeLabel=IRILabel(IRI("http://example.org/S"))))),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse(":x@Start",
      ShapeMap(List(Association(nodeSelector = RDFNodeSelector(IRI("http://default.org/x")),shapeLabel=Start))),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse(":x@ :S",
      ShapeMap(List(Association(nodeSelector = RDFNodeSelector(IRI("http://default.org/x")),shapeLabel=IRILabel(IRI("http://default.shapes.org/S"))))),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse("\"hi\"@es @ :S",
      ShapeMap(List(Association(nodeSelector = RDFNodeSelector(LangLiteral("hi", Lang("es"))),shapeLabel=IRILabel(IRI("http://default.shapes.org/S"))))),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse(":x@ ex:S",
      ShapeMap(List(Association(nodeSelector = RDFNodeSelector(IRI("http://default.org/x")),shapeLabel=IRILabel(IRI("http://shapes.org/S"))))),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse("{ FOCUS a :A} @ ex:S",
      ShapeMap(List(Association(
        nodeSelector = TriplePattern(Focus, rdfType, NodePattern(IRI("http://default.org/A"))),
        shapeLabel=IRILabel(IRI("http://shapes.org/S"))))),
      nodesPrefixMap,
      shapesPrefixMap)


    def shouldParse(str: String,
                    expected: ShapeMap,
                    nodesPrefixMap: PrefixMap,
                    shapesPrefixMap: PrefixMap
                   ): Unit = {
      it(s"should parse $str and obtain $expected") {
        Parser.parse(str, nodesPrefixMap, shapesPrefixMap) match {
          case Left(msg) => fail(s"Failed to parse $str: $msg")
          case Right(shapeMap) => shapeMap shouldBe(expected)
        }
      }
    }
  }
}
