package es.weso.shex.spec

import es.weso.rdf.PrefixMap
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{BNode, IRI, RDFNode}
import es.weso.rdf.triples.RDFTriple
import es.weso.shapeMaps
import es.weso.shapeMaps._
import es.weso.shex.{IRILabel => _, _}
import org.scalatest._

class SpecTest extends FunSpec with Matchers with EitherValues {

  describe(s"Spec test") {
    val x = IRI(s"http://example.org/x")
    val p = IRI(s"http://example.org/p")
    val y = IRI(s"http://example.org/y")
    val b = BNode("b1")

    it(s"Should validate OK") {
      val rdf = RDFAsJenaModel.empty.addTriple(RDFTriple(x,p,y)).getOrElse(RDFAsJenaModel.empty)
      val se: ShapeExpr = NodeConstraint(None,Some(IRIKind), None, List(),None)
      val s: ShapeMapLabel = IRILabel(x)
      val conformant = Info(Conformant,None,None)
      val map = FixedShapeMap(Map(x -> Map(s -> conformant)), PrefixMap.empty,PrefixMap.empty)
      Spec.satisfies(x,se,rdf,map) should be(Right(true))
      Spec.satisfies(b,se,rdf,map) should be(Right(false))
    }
  }
}
