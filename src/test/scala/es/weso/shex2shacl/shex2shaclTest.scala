package es.weso.shex.converter
import es.weso._
import es.weso.shacl._
import es.weso.rdf.nodes._
import org.scalatest._
import es.weso.shacl.converter._

class shex2shaclTest extends FunSpec with Matchers with EitherValues {

  describe("Shex2Shacl") {
    it ("Should convert simple node constraint") {
      val shexSchema: shex.Schema =
        shex.Schema.empty.
        copy(shapes = Some(Map(
             shex.IRILabel(IRI("http://example.org/S")) ->
              shex.NodeConstraint.empty.copy(
                  nodeKind = Some(shex.BNodeKind))
         ))
      )

      val shaclSchema: shacl.Schema = 
        shacl.Schema(Seq(shacl.Shape.empty.copy(
          id = Some(IRI("http://example.org/S")),
          constraints = Seq(
            NodeConstraint(
              List(NodeKind(BlankNodeKind))
            ))
        )))

      val r = ShEx2Shacl.shex2Shacl(shexSchema)
      r.fold(
        e => fail(s"Conversion failed: error: $e"),
        v => v should be(shaclSchema)
      )
    }
  }


  describe("shacl2Shex") {
    it ("Should convert simple node constraint") {
      val shexSchema: shex.Schema =
        shex.Schema.empty.
        copy(
            shapes = Some(Map(
              shex.IRILabel(IRI("http://example.org/S")) ->
              shex.NodeConstraint.empty.copy(
                  nodeKind = Some(shex.BNodeKind))
         ))
      )

      val shaclSchema: shacl.Schema = 
        shacl.Schema(Seq(shacl.Shape.empty.copy(
          id = Some(IRI("http://example.org/S")),
          constraints = Seq(
            NodeConstraint(
              List(NodeKind(BlankNodeKind))
            ))
        )))

      val r = Shacl2ShEx.shacl2ShEx(shaclSchema)
      r.fold(
        e => fail(s"Conversion failed: error: $e"),
        v => v should be(shexSchema)
      )
    }
  }
}
