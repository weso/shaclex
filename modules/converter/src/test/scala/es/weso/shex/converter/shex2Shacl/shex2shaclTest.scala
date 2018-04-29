package es.weso.shex.converter.uml

import es.weso._
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes._
import es.weso.shacl._
import es.weso.shacl.converter._
import es.weso.shex.converter.ShEx2Shacl
import org.scalatest._

class shex2shaclTest extends FunSpec with Matchers with EitherValues {

  describe("Shex2Shacl") {
    ignore("Should convert simple node constraint") {
      val shexSchema: shex.Schema =
        shex.Schema.empty.
          copy(shapes = Some(List(
            shex.NodeConstraint.empty.copy(
              id = Some(shex.IRILabel(IRI("http://example.org/S"))),
              nodeKind = Some(shex.BNodeKind)))))

      val iriS = IRI("http://example.org/S")

      val shaclSchema: shacl.Schema =
        shacl.Schema(
          pm = PrefixMap.empty,
          shapesMap = Map(ShapeRef(iriS) ->
            shacl.Shape.empty(iriS).copy(
              components = List(NodeKind(BlankNodeKind)))))

      val r = ShEx2Shacl.shex2Shacl(shexSchema)
      r.fold(
        e => fail(s"Conversion failed: error: $e"),
        v => v should be(shaclSchema))
    }
  }

  describe("shacl2Shex") {
    ignore("Should convert simple node constraint") {
      val shexSchema: shex.Schema =
        shex.Schema.empty.
          copy(
            shapes = Some(List(
              shex.NodeConstraint.empty.copy(
                id = Some(shex.IRILabel(IRI("http://example.org/S"))),
                nodeKind = Some(shex.BNodeKind)))))

      val iriS = IRI("http://example.org/S")

      val shaclSchema: shacl.Schema =
        shacl.Schema(
          pm = PrefixMap.empty,
          shapesMap = Map(ShapeRef(iriS) -> shacl.Shape.empty(iriS).copy(
            components = List(NodeKind(BlankNodeKind)))))

      val r = Shacl2ShEx.shacl2ShEx(shaclSchema)
      r.fold(
        e => fail(s"Conversion failed: error: $e"),
        v => v should be(shexSchema))
    }
  }

}
