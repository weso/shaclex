package es.weso.shacl

import org.scalatest._
import org.scalatest.matchers._
import es.weso.rdf.nodes._

trait SchemaMatchers {

  class ContainShapesMatcher(shapeIDs: Set[IRI]) extends Matcher[Schema] {

    def apply(schema: Schema) = {
      val iris = schema.shapes.
         map(shape => shape.id).
         filter(_.isDefined).
         map(_.get).toSet

      MatchResult(
        iris == shapeIDs,
        s"Schema $schema + has IDs $iris which is different to expected $shapeIDs",
        s"Schema $schema + has IDs $iris which is equal to $shapeIDs"
      )
    }
  }

  def constainShapes(shapeIDs: Set[IRI]) =
    new ContainShapesMatcher(shapeIDs)

}

// Make them easy to import with:
// import es.weso.shacl.SchemaMatchers._
object SchemaMatchers extends SchemaMatchers
