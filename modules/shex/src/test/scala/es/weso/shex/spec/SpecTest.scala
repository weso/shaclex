package es.weso.shex.spec

import es.weso.rdf.PrefixMap
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.triples.RDFTriple
import es.weso.shapeMaps.{Conformant => SMConformant, _}
import es.weso.shex.validator.Arc
import es.weso.shex.{IRILabel => ShExIriLabel, _}
import org.scalatest._

class SpecTest extends FunSpec with Matchers with EitherValues {
  val x = IRI(s"http://example.org/x")
  val p = IRI(s"http://example.org/p")
  val q = IRI(s"http://example.org/q")
  val r = IRI(s"http://example.org/r")
  val y = IRI(s"http://example.org/y")
  val s = IRI(s"http://example.org/s")
  val b = BNode("b1")
  val slbl = IRILabel(s)
  val lbl = ShExIriLabel(IRI(s"http://example.org/lbl"))
  val lbl2 = ShExIriLabel(IRI(s"http://example.org/lbl2"))
  val emptyEnv = Env(Schema.empty, TypingMap.empty, RDFAsJenaModel.empty)

/*  describe(s"getMatchables") {

    shouldGetMatchables(
      Set(Arc(Direct(p),x), Arc(Direct(q),y)),
      TripleConstraint.emptyPred(p),
      Set(Arc(Direct(p),x))
    )

    shouldGetMatchables(
      Set(Arc(Direct(p),x), Arc(Direct(q),y)),
      EachOf.fromExpressions(TripleConstraint.emptyPred(p), TripleConstraint.emptyPred(p)),
      Set(Arc(Direct(p),x))
    )

    shouldGetMatchables(
      Set(Arc(Direct(p),x), Arc(Direct(q),y)),
      OneOf.fromExpressions(TripleConstraint.emptyPred(p), TripleConstraint.emptyPred(q)),
      Set(Arc(Direct(p),x), Arc(Direct(q),y))
    )


    def shouldGetMatchables(as: Set[Arc], te: TripleExpr, expected: Set[Arc]): Unit = {
      it(s"Should get matchables of $as with $te and return $expected") {
        Check.runCheck(emptyEnv, Spec.getMatchables(as,te)).fold( e => fail(s"Error: $e"),
          vs => vs should contain theSameElementsAs(expected))
      }
    }
  }

  describe(s"notSatisfyMatchables") {

    shouldNotSatisfyMatchables(
      Set(Arc(Direct(p),x)),
      List(TripleConstraint.emptyPred(p), TripleConstraint.emptyPred(q)),
      emptyEnv,
      false)

    shouldNotSatisfyMatchables(
      Set(Arc(Direct(r),x)),
      List(TripleConstraint.emptyPred(p), TripleConstraint.emptyPred(q)),
      emptyEnv,
      true)

    def shouldNotSatisfyMatchables(arcs: Set[Arc], tcs: List[TripleConstraint], env: Env, expected: Boolean): Unit = {
      Check.runCheck(env,Spec.notSatisfyMatchablesTCs(arcs,tcs)).fold(e => fail(s"Error: $e"),
        v => v should be(expected)
      )
    }
  }

  describe(s"Spec test") {

    it(s"Should validate OK") {
      val se: ShapeExpr = NodeConstraint(None, Some(IRIKind), None, List(), None)
//      val s: ShapeMapLabel = IRILabel(x)
//      val conformant = Info(SMConformant, None, None)
//      val map = FixedShapeMap(Map(x -> Map(s -> conformant)), PrefixMap.empty, PrefixMap.empty)
      Check.runCheck(emptyEnv, Spec.satisfies(x, se)) should be(Right(true))
      Check.runCheck(emptyEnv, Spec.satisfies(b, se)) should be(Right(false))
    }

    it(s"Should validate ShapeRef") {
 //     val rdf = RDFAsJenaModel.empty.addTriple(RDFTriple(x, p, y)).getOrElse(RDFAsJenaModel.empty)
      val se1: ShapeExpr = NodeConstraint(Some(lbl), Some(IRIKind), None, List(), None)
      val se2: ShapeExpr = ShapeRef(lbl)
      val se3: ShapeExpr = ShapeRef(lbl2)

      val schema = Schema.empty.addShape(se1).addShape(se2)
 //     val s: ShapeMapLabel = IRILabel(x)
 //     val conformant = Info(SMConformant, None, None)
 //     val map = FixedShapeMap(Map(x -> Map(s -> conformant)), PrefixMap.empty, PrefixMap.empty)
      val env = emptyEnv.copy(schema = schema)
      Check.runCheck(env, Spec.satisfies(x, se2)) should be(Right(true))
      Check.runCheck(env, Spec.satisfies(b, se2)) should be(Right(false))
      Check.runCheck(env, Spec.satisfies(b, se3)).fold(
        e => info(s"Failed with $e as expected"),
        v => fail(s"Returned $v but should have failed")
      )
    }

    it(s"Should validate TripleConstraint with (x,p,y)") {
      val rdf = RDFAsJenaModel.empty.addTriple(RDFTriple(x, p, y)).getOrElse(RDFAsJenaModel.empty)
      val tc = TripleConstraint(None, None, None, p, None, Some(1), Some(IntMax(1)), None, None, None)
      val se = Shape(Some(lbl), None, None, None, Some(tc), None, None, None)
      val schema = Schema.empty.addShape(se)
      val env = emptyEnv.copy(schema = schema, rdf = rdf)
      val slbl: ShapeMapLabel = IRILabel(s)
      val conformant = Info(SMConformant, None, None)
      val map = FixedShapeMap(Map(x -> Map(slbl -> conformant)), PrefixMap.empty, PrefixMap.empty)
      Check.runCheck(env, Spec.satisfies(x, se)) should be(Right(true))

      val map2 = FixedShapeMap(Map(y -> Map(slbl -> conformant)), PrefixMap.empty, PrefixMap.empty)
      Check.runCheck(env, Spec.satisfies(y, se)) should be(Right(false))
    }
*/
  ignore(s"Spec with shape maps") {
/*    shouldValidateShapeMap(
      """|prefix : <http://example.org/>
         |:x :p 1; :q 1 .""".stripMargin,
      """|prefix : <http://example.org/>
         |:S { :p . }""".stripMargin
      , ":x@:S",
      ":x@:S"
    )

    shouldValidateShapeMap(
      """|prefix : <http://example.org/>
         |:x :p 1; :q 1 .""".stripMargin,
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p xsd:integer }""".stripMargin
      , ":x@:S",
      ":x@:S")

    shouldValidateShapeMap(
      """|prefix : <http://example.org/>
         |:x :p "hi"; :q 1 .""".stripMargin,
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p xsd:integer }""".stripMargin
      , ":x@:S",
      ":x@!:S")

    shouldValidateShapeMap(
      """|prefix : <http://example.org/>
         |:x :p 1,2,3; :q true .""".stripMargin,
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p xsd:integer{2,3} ;
         |     :q xsd:boolean }""".stripMargin
      , ":x@:S",
      ":x@:S")

    shouldValidateShapeMap(
      """|prefix : <http://example.org/>
         |:x :p 1,2,3;
         |   :q "wrong" .""".stripMargin,
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p xsd:integer{2,3} ;
         |     :q xsd:boolean }""".stripMargin
      , ":x@:S",
      ":x@!:S")

    shouldValidateShapeMap(
      """|prefix : <http://example.org/>
         |:x :p 1,2,3;
         |   :q "wrong" .""".stripMargin,
      """|prefix : <http://example.org/>
         |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
         |:S { :p xsd:integer{2,3} |
         |     :q xsd:boolean
         |   }""".stripMargin
      , ":x@:S",
      ":x@!:S")
*/
    shouldValidateShapeMap(
      """|<x> <p> 1; <q> 1 .""".stripMargin,
      """|<S> { <p> . |
         |      <q> .
         |   }""".stripMargin
      , "<x>@<S>",
      "<x>@!<S>")
  }

  def shouldValidateShapeMap(strRDF: String, strSchema: String, strShapeMap: String, strExpectedShapeMap: String): Unit = {
    it(s"Should validate RDF\n$strRDF\nSchema:\n$strSchema\nShapeMap:$strShapeMap\nExpected shape map: $strExpectedShapeMap") {
        val r = for {
          rdf <- RDFAsJenaModel.fromChars(strRDF, "Turtle", None)
          schema <- Schema.fromString(strSchema, "ShExC", None)
          shapeMap <- ShapeMap.fromString(strShapeMap, "Compact", None, rdf.getPrefixMap, schema.prefixMap)
          fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, schema.prefixMap)
          shapeTyping <- Check.runCheck(Env(schema, TypingMap.empty, rdf), Spec.checkShapeMap(rdf, fixedShapeMap))
          result = Spec.shapeTyping2ResultShapeMap(shapeTyping,rdf.getPrefixMap,schema.prefixMap)
          expectedShapeMap <- ShapeMap.parseResultMap(strExpectedShapeMap, None, rdf, schema.prefixMap)
          compare <- result.compareWith(expectedShapeMap)
        } yield compare

        r.fold(
          e => fail(s"Error $e"),
          comparison => comparison should be (true)
        )
      }

    }
  // }
}
