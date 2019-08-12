package es.weso.shex.validator
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.IRI
import es.weso.shapeMaps.ShapeMap
import es.weso.shex.Schema
import org.scalatest._

import scala.util._

trait ShouldValidateShapeMap extends FunSpecLike with Matchers {


  def shouldValidateWithShapeMap(
                                  rdfStr: String,
                                  shexStr: String,
                                  shapeMapStr: String,
                                  expected: String): Unit = {
    it(s"Should validate shapeMap: ${shapeMapStr} and return: $expected\nUsing RDF: \n ${rdfStr}\nand schema:\n${shexStr}") {
      val validate = for {
        rdf <- RDFAsJenaModel.fromChars(rdfStr, "Turtle",None)
        shex <- Schema.fromString(shexStr, "ShExC", Some(IRI("")))
        shapeMap <- {
          ShapeMap.fromCompact(shapeMapStr, shex.base, rdf.getPrefixMap, shex.prefixMap)
        }
        fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, shex.prefixMap)
        result <- Validator.validate(shex, fixedShapeMap, rdf)
        expectedShapeMap <- ShapeMap.parseResultMap(expected, shex.base, rdf, shex.prefixMap)
        _ <- { info(s"Expected shapeMap parsed: $expectedShapeMap"); Right(())}
        compare <- result.compareWith(expectedShapeMap)
      } yield compare
      validate match {
        case Left(msg) => fail(s"Error: $msg")
        case Right(v) => v should be(true)
      }
    }
  }

}