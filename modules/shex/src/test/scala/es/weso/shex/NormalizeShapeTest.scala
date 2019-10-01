package es.weso.shex

import es.weso.rdf.nodes._
import es.weso.shex.normalized.{Constraint, NormalizedShape}
import org.scalatest._

class NormalizeShapeTest extends FunSpec with Matchers with EitherValues {

/*  describe(s"Normalize shape simple") {
      val shexStr =
        """
          |prefix : <http://example.org/>
          |:S { :p IRI }
          |""".stripMargin

      val ex = IRI("http://example.org/")
      val p: Path = Direct(ex + "p")
      val a = ex + "S"
      val iri = NodeConstraint.nodeKind(IRIKind,List())
      shouldNormalizeShape(shexStr, a, NormalizedShape(
         Map(p -> Constraint.scala(Some(iri),false,Cardinality(1,IntMax(1)))),
         false)
      )
  }
*/
  describe(s"Normalize shape with OR") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S { :p IRI Or BNode }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val shape = ex + "S"
    // TODO. Check if we could normalize these kind of shapes
    val p: Path = Direct(ex + "p")
    val a = ex + "S"
    val iri = NodeConstraint.nodeKind(IRIKind,List())
    val bNode = NodeConstraint.nodeKind(BNodeKind,List())
    val or = ShapeOr(None,List(iri,bNode),None,None)
    val tc: TripleConstraint = TripleConstraint.valueExpr(ex+"p", or)

    shouldNormalizeShape(shexStr, a,
      NormalizedShape(Map(p -> Vector(Constraint(Some(or),false,Cardinality(1,IntMax(1)), None, tc))), false)
    )
  }

  describe(s"Normalize shape with EXTRA") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S EXTRA :p { :p IRI }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val a = ex + "S"
    val iri = NodeConstraint.nodeKind(IRIKind,List())
    val tc: TripleConstraint = TripleConstraint.valueExpr(ex+"p", iri)

    shouldNormalizeShape(shexStr, a,
      NormalizedShape(Map(p -> Vector(Constraint(Some(iri),true,Cardinality(1,IntMax(1)), None, tc))), false)
    )
  }

  describe(s"Normalize shape simple") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S { :p IRI }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val a = ex + "S"
    val iri = NodeConstraint.nodeKind(IRIKind,List())
    val tc = TripleConstraint.valueExpr(ex + "p",iri)
    shouldNormalizeShape(shexStr, a,
      NormalizedShape(Map(p -> Vector(normalized.Constraint(Some(iri),false,Cardinality(1,IntMax(1)),None,tc))), false)
    )
  }

  describe(s"Normalize shape with annotation ") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S {
        | :p IRI // :a 1
        |}
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val a = ex + "S"
    val iri = NodeConstraint.nodeKind(IRIKind,List())
    val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
    val as = Some(List(Annotation(ex + "a",DatatypeString("1",xsd + "integer"))))
    val tc = TripleConstraint.valueExpr(ex + "p",iri).copy(annotations = as)
    shouldNormalizeShape(shexStr, a,
      NormalizedShape(Map(p -> Vector(normalized.Constraint(Some(iri),false,Cardinality(1,IntMax(1)),as,tc))), false)
    )
  }

  describe(s"Normalize shape with top level annotation ") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S IRI {
        | :p IRI // :a 2
        |}
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val a = ex + "S"
    val iri = NodeConstraint.nodeKind(IRIKind,List())
    val xsd = IRI("http://www.w3.org/2001/XMLSchema#")
    val as = Some(List(Annotation(ex + "a",DatatypeString("1",xsd + "integer"))))
    val tc = TripleConstraint.valueExpr(ex + "p",iri).copy(annotations = as)

    // TODO: Add a test to check normalized shape

  }

  def shouldNormalizeShape(strSchema: String, shapeLabel: IRI, ns: NormalizedShape): Unit = {
    it(s"Should normalize $shapeLabel and return $ns") {
      val shapeLbl = IRILabel(shapeLabel)
      val result = for {
        schema <- Schema.fromString(strSchema)
        shape <- schema.getShape(shapeLbl)
        normalized <- shape match {
          case s: Shape => s.normalized(Schema.empty)
          case _ => Left(s"$shape is not a plain shape")
        }
      } yield normalized
      result.fold(e => fail(s"Error: $e"),
        n => n should be(ns)
      )
    }
  }

  def shouldNotNormalizeShape(strSchema: String, shapeLabel: IRI): Unit = {
    it(s"Should not normalize $shapeLabel") {
      val shapeLbl = IRILabel(shapeLabel)
      val result = for {
        schema <- Schema.fromString(strSchema)
        shape <- schema.getShape(shapeLbl)
        normalized <- shape match {
          case s: Shape => s.normalized(Schema.empty)
          case _ => Left(s"$shape is not a plain shape")
        }
      } yield normalized
      result.fold(e => info(s"Could not normalize shape with error $e as expected"),
        n => fail(s"It was able to normalize shape and return $n but it should have failed")
      )
    }
  }
}
