package es.weso.shex

import es.weso.rdf.nodes._
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
         Map(p -> Constraint(Some(iri),false,Cardinality(1,IntMax(1)))),
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
    shouldNotNormalizeShape(shexStr, shape)
  }

  describe(s"Normalize shape with OR") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S { :p IRI }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val a = ex + "S"
    val iri = NodeConstraint.nodeKind(IRIKind,List())
    shouldNormalizeShape(shexStr, a,
      NormalizedShape(Map(p -> Constraint(Some(iri),false,Cardinality(1,IntMax(1)))), false)
    )
  }
  /*
  describe(s"Normalize shape simple with two predicates") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S {
        |  :p IRI ;
        |  :q IRI {2,5};
        |  :r . *
        |}
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p: Path = Direct(ex + "p")
    val q: Path = Direct(ex + "q")
    val r: Path = Direct(ex + "r")
    val a = ex + "S"
    val iri = Some(NodeConstraint.nodeKind(IRIKind,List()))
    val dot = Some(Shape.empty)
    shouldNormalizeShape(shexStr, a, NormalizedShape(
      Map(
        p -> Constraint(iri,false,Cardinality(1,IntMax(1))),
        q -> Constraint(iri,false,Cardinality(2,IntMax(5))),
        r -> Constraint(dot,false,Cardinality(0,Star))
      ),
      false)
    )
  }
*/
  def shouldNormalizeShape(strSchema: String, shapeLabel: IRI, ns: NormalizedShape) = {
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

  def shouldNotNormalizeShape(strSchema: String, shapeLabel: IRI) = {
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
