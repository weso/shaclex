package es.weso.shacl

// import org.scalatest._
import org.scalatest.matchers._
import es.weso.rdf.nodes._

trait SchemaMatchers {

  class ContainShapesMatcher(shapeIDs: Set[RDFNode]) extends Matcher[Schema] {

    def apply(schema: Schema) = {
      val shapes = schema.shapes.map(_.id)

      MatchResult(
        shapeIDs == shapes.toSet,
        s"Schema $schema + has shapes $shapes which is different to expected $shapeIDs",
        s"Schema $schema + has shapes $shapes which is equal to $shapeIDs")
    }
  }

  def containShapes(shapeIDs: Set[RDFNode]) =
    new ContainShapesMatcher(shapeIDs)

}

// Make them easy to import with:
// import es.weso.shacl.SchemaMatchers._
object SchemaMatchers extends SchemaMatchers
