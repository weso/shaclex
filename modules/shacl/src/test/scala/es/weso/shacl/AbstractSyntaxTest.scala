package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.path.PredicatePath

class AbstractSyntaxTest extends FunSpec with Matchers {

  describe("Abstract Syntax") {
    it("should be able to create a shape") {
      val x = BNode("x")
      val c: PropertyShape =
        PropertyShape(
          id = x,
          path = PredicatePath(IRI("http://example.org/p")),
          components =
            List(
              NodeKind(IRIKind),
              MinCount(1),
              MaxCount(2)),
          targets = Seq(),
          propertyShapes = Seq(),
          closed = false,
          ignoredProperties = List())
      val id = IRI("http://example.org/s")
      val shape = NodeShape(
        id = id,
        components = List(),
        targets = List(),
        propertyShapes = List(ShapeRef(x)),
        false,
        List())

      shape.id should be(id)

    }

  }
}
