package es.weso.shex

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import org.scalatest._

class PathsTest extends FunSpec with Matchers with EitherValues {

  describe(s"Calculates paths of a shape") {
      val shexStr =
        """
          |prefix : <http://example.org/>
          |:A { $<lbl> (:p .; :q .) }
          |:B { :r . ; &<lbl> }
          |""".stripMargin

      val ex = IRI("http://example.org/")
      val p = Direct(ex + "p")
      val q = Direct(ex + "q")
      val r = Direct(ex + "r")
      val a = ex + "A"
      val b = ex + "B"
      shouldMatchPaths(shexStr,a,List(p,q))
      shouldMatchPaths(shexStr,b,List(p,q,r))
  }

  def shouldMatchPaths(strSchema: String, shapeLabel: IRI, paths: List[Path]) = {
    it(s"Should calculate paths of shape $shapeLabel and return $paths") {
      val shapeLbl = IRILabel(shapeLabel)
      val result = for {
        schema <- Schema.fromString(strSchema)
        shape <- schema.getShape(shapeLbl)
        paths <- shape.paths(schema)
      } yield paths
      result.fold(e => fail(s"Error: $e"),
        ps => ps should contain theSameElementsAs (paths)
      )
    }
  }

}
