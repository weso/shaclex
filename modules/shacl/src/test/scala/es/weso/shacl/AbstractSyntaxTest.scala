package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._

class AbstractSyntaxTest extends FunSpec with Matchers {

  describe("Abstract Syntax") {
    it("should be able to create a shape") {
      val x = BNode("x")
      val id = IRI("http://example.org/s")
      val shape = NodeShape(
        id = id,
        components = List(),
        targets = List(),
        propertyShapes = List(RefNode(x)),
        false,
        List(),
        false,
        MessageMap.empty,
        None,
        name = MessageMap.empty,
        description = MessageMap.empty,
        order = None,
        group = None,
        sourceIRI = None
      )

      shape.id should be(id)

    }

  }
}
