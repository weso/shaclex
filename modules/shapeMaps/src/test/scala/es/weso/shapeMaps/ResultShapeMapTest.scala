package es.weso.shapeMaps

import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import org.scalatest._

class ResultShapeMapTest extends FunSpec with Matchers with TryValues with OptionValues {

/*  describe("ResultShapeMaps") {
    val rdfStr =
      """prefix : <http://example.org/>
        |:a :b :c .
      """.stripMargin

    compareResultMaps(":a@:S", ":a@:S", rdfStr, true)
    compareResultMaps(":a@:S", ":b@:S", rdfStr, false)
    compareResultMaps(":a@:S", ":a@:S,:b@:S", rdfStr, false)
    compareResultMaps(":a@!:S", ":a@!:S", rdfStr, true)

  }

  def compareResultMaps(strMap1: String, strMap2: String, rdfStr: String, expectedEqual: Boolean): Unit = {
    it(s"Should compare $strMap1 with $strMap2 and equal=$expectedEqual") {
      val eitherResult = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr, "Turtle")
        map1 <- ShapeMap.parseResultMap(strMap1, None, rdf, rdf.getPrefixMap)
        map2 <- ShapeMap.parseResultMap(strMap2, None, rdf, rdf.getPrefixMap)
      } yield (map1, map2)
      eitherResult.fold(
        err => fail(s"Error comparing maps: $err"),
        pair => {
          val (m1, m2) = pair
          m1 compareWith m2 match {
            case Left(err) =>
              if (expectedEqual) fail(s"Expected equal but are different: $err")
              else ()
            case Right(_) =>
              if (expectedEqual) ()
              else fail(s"Expected different, but are equal")
          }
        })
    }
  }
*/
  describe(s"Parse result shape map") {
    it(s"Should parse result shape map") {
      val rdfStr =
        """|prefix : <http://example.org/>
          |:x :p 1 .
        """.stripMargin

      val result = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr,"TURTLE",None)
        resultMap <- ShapeMap.parseResultMap(":x@!:S", None, rdf, rdf.getPrefixMap)
      } yield (rdf,resultMap)

      result match {
        case Left(err) => fail(s"Error: $err")
        case Right((rdf,result)) => {
          val x = IRI("http://example.org/x")
          val s = IRILabel(IRI("http://example.org/S"))
          val r: ResultShapeMap = ResultShapeMap(Map(x -> Map(s -> Info(NonConformant, None, None))), rdf.getPrefixMap, rdf.getPrefixMap)
          println(s"Result: $result\nr=$r")
          result should be(r)
        }
      }
    }
  }

/*  describe(s"Get conformant shapes") {
    val pm = PrefixMap.empty.addPrefix("",IRI("http://example.org/"))
    val x: RDFNode = IRI("http://example.org/x")
    val y: RDFNode = IRI("http://example.org/y")
    val z: RDFNode = IRI("http://example.org/z")
    val s: ShapeMapLabel = IRILabel(IRI("http://example.org/s"))
    val t: ShapeMapLabel = IRILabel(IRI("http://example.org/t"))
    val u: ShapeMapLabel = IRILabel(IRI("http://example.org/u"))
    val conformant = Info(Conformant, Some("ok"),None)
    val nonConformant = Info(NonConformant, Some("fail"), None)
    val rm = ResultShapeMap(
      Map(x -> Map(s -> conformant, t -> nonConformant),
        y -> Map(t -> conformant),
        z -> Map(t -> nonConformant) ), pm, pm
    )
    it("should get conformant shapes of x ") {
      rm.getConformantShapes(x) should contain theSameElementsAs (List(s))
      rm.getNonConformantShapes(x) should contain theSameElementsAs (List(t))
    }
    it("should get conformant shapes of y ") {
      rm.getConformantShapes(y) should contain theSameElementsAs (List(t))
      rm.getNonConformantShapes(y) should contain theSameElementsAs (List())
    }
    it("should get conformant shapes of z ") {
      rm.getConformantShapes(z) should contain theSameElementsAs (List())
      rm.getNonConformantShapes(z) should contain theSameElementsAs (List(t))
    }
    it("should get info about x") {
      rm.getInfo(x,s).status should be(Conformant)
      rm.getInfo(x,t).status should be(NonConformant)
      rm.getInfo(x,u).status should be(Undefined)
    }
    it("should get info about y") {
      rm.getInfo(y,s).status should be(Undefined)
      rm.getInfo(y,t).status should be(Conformant)
      rm.getInfo(y,u).status should be(Undefined)
    }
    it("should get info about z") {
      rm.getInfo(z,s).status should be(Undefined)
      rm.getInfo(z,t).status should be(NonConformant)
      rm.getInfo(z,u).status should be(Undefined)
    }
  } */
}
