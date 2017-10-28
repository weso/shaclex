package es.weso.shapeMaps

import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import org.scalatest._

import scala.util.{ Failure, Success }

class ResultShapeMapTest extends FunSpec with Matchers with TryValues with OptionValues {

  describe("ResultShapeMaps") {
    val rdfStr =
      """prefix : <http://example.org/>
        |:a :b :c .
      """.stripMargin
    compareResultMaps(":a@:S", ":a@:S", rdfStr, true)
    compareResultMaps(":a@:S", ":b@:S", rdfStr, false)
    compareResultMaps(":a@:S", ":a@:S,:b@:S", rdfStr, false)
  }

  def compareResultMaps(strMap1: String, strMap2: String, rdfStr: String, expectedEqual: Boolean): Unit = {
    it(s"Should compare $strMap1 with $strMap2 and equal=$expectedEqual") {
      val eitherResult = for {
        rdf <- RDFAsJenaModel.parseChars(rdfStr, "Turtle")
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
}
