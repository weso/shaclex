package es.weso.rdf.operations

import cats.implicits._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes._
import es.weso.rdf.triples.RDFTriple
import org.scalatest._

class GraphTest extends FunSpec with Matchers with TryValues {

  def ex: IRI = IRI("http://example.org/")
  def iri(s: String): IRI = ex + s
  def bnode(s: String): BNode = BNode(s)
  def int(n: Int): IntegerLiteral = IntegerLiteral(n)

  describe("Graph Traverse") {

    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
        |:x :p :y .
      """.stripMargin,
      List(iri("x"), iri("y"))
    )
    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x .
      """.stripMargin,
      List(iri("x"))
    )
    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x, :y .
         |:y :p :z, :x .
      """.stripMargin,
      List(iri("x"), iri("y"), iri("z"))
    )
    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x, :y .
         |:y :p :z, :x .
         |:r :q :x .
      """.stripMargin,
      List(iri("x"), iri("y"), iri("z"))
    )
    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x, :y ;
         |   :q :t .
         |:y :p :z, :x .
         |:r :q :x .
      """.stripMargin,
      List(iri("x"), iri("y"), iri("z"), iri("t"))
    )
    shouldTraverse(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p _:1, _:2 ;
         |   :q 1 .
         |_:1 :p :y, :z .
         |:r :q :x .
      """.stripMargin,
      List(iri("x"), iri("y"), iri("z"), bnode("0"), bnode("1"), int(1))
    )

    def shouldTraverse(node: RDFNode, str: String, expected: List[RDFNode]): Unit = {
      it(s"shouldTraverse(${node.show} in graph ${str}) and return $expected") {
        val r = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", None)
        } yield rdf.normalizeBNodes
       r.fold(e => fail(s"Error: $e"), rdf => {
         val ls = Graph.traverse(node,rdf)
         ls should contain theSameElementsAs(expected)
       })
      }
    }
  }

  describe("Graph Traverse") {
    shouldTraverseWithArcs(
      iri("x"),
      """|prefix : <http://example.org/>
         |:x :p :x, :y .
         |:z :p :x, :y .
      """.stripMargin,
      (List(iri("x"), iri("y")),
      List(
        RDFTriple(iri("x"), iri("p"),iri("y")),
        RDFTriple(iri("x"), iri("p"),iri("x"))
      ))
    )

    def shouldTraverseWithArcs(node: RDFNode,
                               str: String,
                               expected: (List[RDFNode], List[RDFTriple])): Unit = {
      it(s"shouldTraverseWithArcs(${node.show} in graph ${str}) and return $expected") {
        val r = for {
          rdf <- RDFAsJenaModel.fromChars(str, "TURTLE", None)
        } yield rdf.normalizeBNodes
        r.fold(e => fail(s"Error: $e"), rdf => {
          val pair = Graph.traverseWithArcs(node,rdf)
          val (ls,triples) = pair
          val (lsExpected, triplesExpected) = expected
          ls should contain theSameElementsAs(lsExpected)
          triples should contain theSameElementsAs(triplesExpected)
        })
      }
    }
  }
  }