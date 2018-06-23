package es.weso.shex.spec

import es.weso.rdf.PrefixMap
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.triples.RDFTriple
import es.weso.shapeMaps._
import es.weso.shex.{IRILabel => ShExIriLabel, _}
import org.scalatest._

class SpecTest extends FunSpec with Matchers with EitherValues {

  describe(s"Spec test") {
    val x = IRI(s"http://example.org/x")
    val p = IRI(s"http://example.org/p")
    val y = IRI(s"http://example.org/y")
    val b = BNode("b1")
    val lbl = ShExIriLabel(IRI(s"http://example.org/lbl"))
    val lbl2 = ShExIriLabel(IRI(s"http://example.org/lbl2"))

    it(s"Should validate OK") {
      val rdf = RDFAsJenaModel.empty.addTriple(RDFTriple(x,p,y)).getOrElse(RDFAsJenaModel.empty)
      val se: ShapeExpr = NodeConstraint(None,Some(IRIKind), None, List(),None)
      val s: ShapeMapLabel = IRILabel(x)
      val conformant = Info(Conformant,None,None)
      val map = FixedShapeMap(Map(x -> Map(s -> conformant)), PrefixMap.empty,PrefixMap.empty)
      Check.runCheck(Schema.empty, Spec.satisfies(x,se,rdf,map)) should be(Right(true))
      Check.runCheck(Schema.empty, Spec.satisfies(b,se,rdf,map)) should be(Right(false))
    }

    it(s"Should validate ShapeRef") {
      val rdf = RDFAsJenaModel.empty.addTriple(RDFTriple(x,p,y)).getOrElse(RDFAsJenaModel.empty)
      val se1: ShapeExpr = NodeConstraint(Some(lbl),Some(IRIKind), None, List(),None)
      val se2: ShapeExpr = ShapeRef(lbl)
      val se3: ShapeExpr = ShapeRef(lbl2)

      val schema = Schema.empty.addShape(se1).addShape(se2)
      val s: ShapeMapLabel = IRILabel(x)
      val conformant = Info(Conformant,None,None)
      val map = FixedShapeMap(Map(x -> Map(s -> conformant)), PrefixMap.empty,PrefixMap.empty)
      Check.runCheck(schema, Spec.satisfies(x,se2,rdf,map)) should be(Right(true))
      Check.runCheck(schema, Spec.satisfies(b,se2,rdf,map)) should be(Right(false))
      Check.runCheck(schema, Spec.satisfies(b,se3,rdf,map)).fold(
        e => info(s"Failed with $e as expected"),
        v => fail(s"Returned $v but should have failed")
      )
    }
  }
}
