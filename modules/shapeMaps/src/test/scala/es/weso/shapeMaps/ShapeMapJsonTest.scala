package es.weso.shapeMaps

import es.weso.json.JsonCompare
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.path.PredicatePath
import org.scalatest._

import scala.util.{ Failure, Success }

class ShapeMapJsonTest extends FunSpec with Matchers with TryValues with OptionValues {

  describe("Json conversion from/to ShapeMaps") {
    val npm = PrefixMap.empty.addPrefix("", IRI("http://example.org/"))
    val spm = npm
    encodeDecodeJson("<http://example.org/user>@<http://example.org/User>", npm, spm)
    encodeDecodeJson("{ _ a FOCUS }@:User", npm, spm)
  }

  def encodeDecodeJson(str: String, nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): Unit = {
    it(s"Should encodeDecode as Json $str") {
      val result = for {
        shapeMap <- ShapeMap.fromString(str, nodesPrefixMap, shapesPrefixMap)
        json = {
          println(s"ShapeMap: $shapeMap")
          shapeMap.toJson
        }
        shapeMapFromJson <- {
          println(s"Json: ${json.spaces2}")
          ShapeMap.fromJson(json.spaces2)
        }
      } yield (json, shapeMapFromJson.toJson)
      result match {
        case Left(str) => fail(s"Error encoding/decoding Json: $str")
        case Right((json1, json2)) =>
          if (json1 == json2) info(s"Encoded/decoded $str")
          else fail(s"${json1.spaces2} != ${json2.spaces2}. Diff: ${JsonCompare.diff(json1, json2)}")
      }
    }

  }
}
