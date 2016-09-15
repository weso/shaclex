package es.weso.shex.converter
import es.weso._
import es.weso.shacl._
import es.weso.rdf.nodes._
import org.scalatest._

class shex2shaclTest extends FunSpec with Matchers with EitherValues {

  describe("Shex2Shacl") {
    it ("Should convert simple node constraint") {
      val schema1: shex.Schema =
        shex.Schema.empty.
        copy(prefixes = Some(Map(shex.Prefix("") -> IRI("http://example.org/"))),
             shapes = Some(Map(
            shex.IRILabel(IRI("http://example.org/S")) ->
              shex.NodeConstraint.empty.copy(
                  nodeKind = Some(shex.BNodeKind))
         ))
      )

      val expected: shacl.Schema = 
        shacl.Schema(Seq(shacl.Shape.empty.copy(
          id = Some(IRI("http://example.org/S")),
          constraints = Seq(
            NodeConstraint(
              List(NodeKind(BlankNodeKind))
            ))
        )))

      val r = ShEx2Shacl.shex2Shacl(schema1)
      r.fold(
        e => fail(s"Conversion failed: error: $e"),
        v => v should be(expected)
      )
    }
  }
}
