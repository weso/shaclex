package es.weso.shapeMaps

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import es.weso.rdf.path.PredicatePath

import scala.util.{ Failure, Success }

class ShapeMapTest extends FunSpec with Matchers with TryValues with OptionValues {

  describe("ShapeMaps") {

    it("should be able to create a shape map") {
      val map = QueryShapeMap(
        List(Association(node = RDFNodeSelector(IRI("http://example.org/x")), shape = Start)),
        PrefixMap.empty,
        PrefixMap.empty)
    }
  }

  describe("ShapeMaps parser") {

    val nodesPrefixMap = PrefixMap.empty.
      addPrefix("", IRI("http://default.org/")).
      addPrefix("ex", IRI("http://example.org/"))
    val shapesPrefixMap = PrefixMap.empty.
      addPrefix("", IRI("http://default.shapes.org/")).
      addPrefix("ex", IRI("http://shapes.org/"))
    val rdfType = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")

    shouldParse(
      "<http://example.org/x> @ Start",
      QueryShapeMap(
        List(Association(node = RDFNodeSelector(IRI("http://example.org/x")), shape = Start)), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "23@Start",
      QueryShapeMap(
        List(Association(
          node = RDFNodeSelector(IntegerLiteral(23)), shape = Start)),
        nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "<http://example.org/x>@Start",
      QueryShapeMap(List(Association(node = RDFNodeSelector(IRI("http://example.org/x")), shape = Start)), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "<http://example.org/x>@<http://example.org/S>",
      QueryShapeMap(
        List(Association(
          node = RDFNodeSelector(IRI("http://example.org/x")),
          shape = IRILabel(IRI("http://example.org/S")))),
        nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse(
      ":x@Start",
      QueryShapeMap(List(Association(node = RDFNodeSelector(IRI("http://default.org/x")), shape = Start)), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse(
      ":x@ :S",
      QueryShapeMap(
        List(
          Association(
            node = RDFNodeSelector(IRI("http://default.org/x")),
            shape = IRILabel(IRI("http://default.shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)
    /*    Ignore this test until we solve issue #48
      https://github.com/labra/shaclex/issues/48

      shouldParse(
      "\"hi\"@es @ :S",
      QueryShapeMap(List(Association(
        node = RDFNodeSelector(LangLiteral("hi", Lang("es"))),
        shape = IRILabel(IRI("http://default.shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap) */
    shouldParse(
      ":x@ ex:S",
      QueryShapeMap(List(Association(
        node = RDFNodeSelector(IRI("http://default.org/x")),
        shape = IRILabel(IRI("http://shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "{ FOCUS a :A} @ ex:S",
      QueryShapeMap(List(Association(
        node = TriplePattern(Focus, PredicatePath(rdfType), NodePattern(IRI("http://default.org/A"))),
        shape = IRILabel(IRI("http://shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "{FOCUS :p _ }@ :S",
      QueryShapeMap(List(Association(
        node = TriplePattern(Focus, PredicatePath(IRI("http://default.org/p")), WildCard),
        shape = IRILabel(IRI("http://default.shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    def shouldParse(
      str: String,
      expected: ShapeMap,
      nodesPrefixMap: PrefixMap,
      shapesPrefixMap: PrefixMap): Unit = {
      it(s"should parse $str and obtain $expected") {
        Parser.parse(str, None, nodesPrefixMap, shapesPrefixMap) match {
          case Left(msg) => fail(s"Failed to parse $str: $msg")
          case Right(shapeMap) => shapeMap shouldBe (expected)
        }
      }
    }
  }

  describe("Fix shape map") {

    val rdfStr =
      """
        |prefix : <http://example.org/>
        |:x a :X ;
        |   :p :y, :z .
        |:t a :X .
        |:u :q :w .
        |:v :q :w .
        |:a :p :y .
        |:b :p :y .
        |:c :p :z .
      """.stripMargin

    shouldFixAs(":x@ :S", rdfStr, ":x@ :S")
    shouldFixAs("{FOCUS a :X }@ :S", rdfStr, ":x@ :S, :t @ :S")
    shouldFixAs("{FOCUS :p :y }@ :S", rdfStr, ":x@ :S, :a@ :S, :b@ :S")
    shouldFixAs("{FOCUS :p _ }@ :S", rdfStr, ":x@ :S, :a@ :S, :b @ :S, :c @ :S")

    def shouldFixAs(shapeMapStr: String, rdfStr: String, expectedStr: String): Unit = {
      val shapesPrefixMap = PrefixMap.empty.addPrefix("", IRI("http://example.org/"))
      it(s"should fix $shapeMapStr and obtain $expectedStr") {
        RDFAsJenaModel.fromChars(rdfStr, "TURTLE") match {
          case Failure(e) => fail(s"Error parsing $rdfStr")
          case Success(rdf) => {
            val result = for {
              shapeMap <- Parser.parse(shapeMapStr, None, rdf.getPrefixMap, shapesPrefixMap)
              expected <- Parser.parse(expectedStr, None, rdf.getPrefixMap, shapesPrefixMap)
              obtained <- ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, shapesPrefixMap)
            } yield (obtained, expected)
            result match {
              case Left(msg) => fail(s"Error $msg fixing map $shapeMapStr")
              case Right((obtained, expected)) =>
                obtained.associations should contain theSameElementsAs (expected.associations)
            }
          }
        }
      }
    }
  }

  describe("Show and parse shapeMaps") {
    val rdfStr =
      """
        |prefix : <http://example.org/>
        |
        |:x a :X .
        |
      """.stripMargin
    shouldShowAndParse(":a @ :S", rdfStr)
    shouldShowAndParse("{FOCUS a :X}@ :S", rdfStr)
    shouldShowAndParse("{FOCUS a _}@ :S", rdfStr)
    shouldShowAndParse("{FOCUS :p :X}@ :S", rdfStr)

    def shouldShowAndParse(shapeMapStr: String, rdfStr: String): Unit = {
      val shapesPrefixMap = PrefixMap.empty.addPrefix("", IRI("http://example.org/"))
      it(s"Should show and parse $shapeMapStr") {
        RDFAsJenaModel.fromChars(rdfStr, "TURTLE") match {
          case Failure(e) => fail(s"Error parsing $rdfStr")
          case Success(rdf) => {
            Parser.parse(shapeMapStr, None, rdf.getPrefixMap, shapesPrefixMap) match {
              case Left(msg) => fail(s"Error parsing ${shapeMapStr}: ${msg}")
              case Right(shapeMap) => Parser.parse(shapeMap.toString, None, rdf.getPrefixMap, shapesPrefixMap) match {
                case Left(msg) => fail(s"Error parsing shown shapeMap ${shapeMap.toString} of ${shapeMapStr}: ${msg}")
                case Right(shownShapeMap) => shapeMap should be(shownShapeMap)
              }
            }
          }
        }
      }
    }
  }
}
