package es.weso.shex.spec

import es.weso.rdf.PrefixMap
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.triples.RDFTriple
import es.weso.shapeMaps.{Conformant => SMConformant, _}
import es.weso.shex.{IRILabel => ShExIriLabel, _}
import es.weso.typing.Typing
import org.scalatest._

class SpecTest extends FunSpec with Matchers with EitherValues {

  val emptyEnv = Env(Schema.empty,TypingMap.empty,RDFAsJenaModel.empty)

  describe(s"Spec test") {
    val x = IRI(s"http://example.org/x")
    val p = IRI(s"http://example.org/p")
    val y = IRI(s"http://example.org/y")
    val s = IRI(s"http://example.org/s")
    val b = BNode("b1")
    val slbl = IRILabel(s)
    val lbl = ShExIriLabel(IRI(s"http://example.org/lbl"))
    val lbl2 = ShExIriLabel(IRI(s"http://example.org/lbl2"))

    it(s"Should validate OK") {
      val rdf = RDFAsJenaModel.empty.addTriple(RDFTriple(x,p,y)).getOrElse(RDFAsJenaModel.empty)
      val se: ShapeExpr = NodeConstraint(None,Some(IRIKind), None, List(),None)
      val s: ShapeMapLabel = IRILabel(x)
      val conformant = Info(SMConformant,None,None)
      val map = FixedShapeMap(Map(x -> Map(s -> conformant)), PrefixMap.empty,PrefixMap.empty)
      Check.runCheck(emptyEnv, Spec.satisfies(x,se)) should be(Right(true))
      Check.runCheck(emptyEnv, Spec.satisfies(b,se)) should be(Right(false))
    }

    it(s"Should validate ShapeRef") {
      val rdf = RDFAsJenaModel.empty.addTriple(RDFTriple(x,p,y)).getOrElse(RDFAsJenaModel.empty)
      val se1: ShapeExpr = NodeConstraint(Some(lbl),Some(IRIKind), None, List(),None)
      val se2: ShapeExpr = ShapeRef(lbl)
      val se3: ShapeExpr = ShapeRef(lbl2)

      val schema = Schema.empty.addShape(se1).addShape(se2)
      val s: ShapeMapLabel = IRILabel(x)
      val conformant = Info(SMConformant,None,None)
      val map = FixedShapeMap(Map(x -> Map(s -> conformant)), PrefixMap.empty,PrefixMap.empty)
      val env = emptyEnv.copy(schema = schema)
      Check.runCheck(env, Spec.satisfies(x,se2)) should be(Right(true))
      Check.runCheck(env, Spec.satisfies(b,se2)) should be(Right(false))
      Check.runCheck(env, Spec.satisfies(b,se3)).fold(
        e => info(s"Failed with $e as expected"),
        v => fail(s"Returned $v but should have failed")
      )
    }

    it(s"Should validate TripleConstraint with (x,p,y)") {
      val rdf = RDFAsJenaModel.empty.addTriple(RDFTriple(x,p,y)).getOrElse(RDFAsJenaModel.empty)
      val tc = TripleConstraint(None,None,None,p,None,Some(1),Some(IntMax(1)),None,None,None)
      val se = Shape(Some(lbl),None,None,None,Some(tc),None,None,None)
      val schema = Schema.empty.addShape(se)
      val env = emptyEnv.copy(schema = schema, rdf = rdf)
      val slbl: ShapeMapLabel = IRILabel(s)
      val conformant = Info(SMConformant,None,None)
      val map = FixedShapeMap(Map(x -> Map(slbl -> conformant)), PrefixMap.empty,PrefixMap.empty)
      Check.runCheck(env, Spec.satisfies(x,se)) should be(Right(true))

      val map2 = FixedShapeMap(Map(y -> Map(slbl -> conformant)), PrefixMap.empty,PrefixMap.empty)
      Check.runCheck(env, Spec.satisfies(y,se)) should be(Right(false))
    }

    it(s"Should validate TripleConstraint") {
      val strRdf =
        """|prefix : <http://example.org/>
           |:x :p 1; :q 1 .
        """.stripMargin
      val strshex =
        """|prefix : <http://example.org/>
          |:S { :p . }
        """.stripMargin
      val strShapemap = ":x@:S"
      val r = for {
        rdf <- RDFAsJenaModel.fromChars(strRdf, "Turtle", None)
        schema <- Schema.fromString(strshex, "ShExC", None, RDFAsJenaModel.empty)
        shapeMap <- ShapeMap.fromString(strShapemap, "Compact",None,rdf.getPrefixMap,schema.prefixMap)
        fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, schema.prefixMap)
        r <- Check.runCheck(Env(schema, TypingMap.empty, rdf), Spec.satisfiesLabel(x, IRILabel(IRI("http://example.org/S"))))
      } yield r
      r.fold(
          e => fail(s"Error $e"),
          v => v should be(true)
        )
      }

    }

}
