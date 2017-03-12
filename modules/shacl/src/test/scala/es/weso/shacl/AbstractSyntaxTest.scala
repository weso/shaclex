package es.weso.shacl

import org.scalatest._
import es.weso.rdf.nodes._
import es.weso.rdf.path.PredicatePath

class AbstractSyntaxTest extends FunSpec with Matchers {

describe("Abstract Syntax") {
  it("should be able to create a shape") {
    val c : PropertyShape =
      PropertyShape(
                id = None,
                path = PredicatePath(IRI("http://example.org/p")),
                components =
                  List(NodeKind(IRIKind),
                       MinCount(1),
                       MaxCount(2)),
                targets = Seq(),
                propertyShapes = Seq(),
        closed = false,
        ignoredProperties = List()
      )

    val shape = NodeShape(
        id = Some(IRI("http://example.org/s")),
        components = List(),
        targets = List(),
        propertyShapes = List(c),
        false,
        List())

    shape.id shouldBe defined

  }

  }
}
