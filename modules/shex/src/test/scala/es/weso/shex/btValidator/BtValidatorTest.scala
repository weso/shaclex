package es.weso.shex.btValidator

import org.scalatest._
import BtValidator._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.PREFIXES._
import es.weso.shapeMaps.{IRILabel, ShapeMapLabel}
import es.weso.shex._

class BtValidatorTest extends FunSpec with Matchers with EitherValues {

  describe(s"BtValidator semantics") {
    it(s"Should validate OK") {
      val c: Check[Int] = ok(3)
      val rdf = RDFAsJenaModel.empty
      val schema = Schema.empty
      val r = runCheck(rdf, schema, c)
      r.fold(e => fail(s"Error: $e"),
        n => n should be(3))
    }
    it(s"Should getRDF") {
      val x = IRI(s"http://example.org/x")
      val y = IRI(s"http://example.org/y")
      val rdf = RDFAsJenaModel.empty.addTriple(RDFTriple(x, `rdf:type`,y)).getOrElse(RDFAsJenaModel.empty)
      val c:Check[Set[RDFNode]] = for {
        rdf <- getRDF
        ts <- fromEither(rdf.getTypes(x))
      } yield ts
      runCheck(rdf,Schema.empty,c).fold(e => fail(s"Error"),
        ns => ns should contain theSameElementsAs (List(y))
      )
    }
    it(s"Should run local computation with updated varTable") {
      val x = IRI(s"http://example.org/x")
      val y = IRI(s"http://example.org/y")
      val rdf = RDFAsJenaModel.empty
      val c:Check[List[RDFNode]] = for {
        table <- getVarTable
      } yield table.get(VarName("v"))
      val c1 = localWithTable(_.set(VarName("v"), List(x,y)),c)
      runCheck(rdf,Schema.empty,c1).fold(e => fail(s"Error $e"),
        ns => ns should contain theSameElementsAs (List(x,y))
      )
    }
    it(s"Should run local computation with updated typing") {
      val x = IRI(s"http://example.org/x")
      val y = IRI(s"http://example.org/y")
      val rdf = RDFAsJenaModel.empty
      val c:Check[Set[ShapeMapLabel]] = for {
        typing <- getTyping
      } yield typing.getOkValues(x)
      val c1 = localWithTyping(_.addEvidence(x,IRILabel(y),List()),c)
      runCheck(rdf,Schema.empty,c1).fold(e => fail(s"Error $e"),
        ns => ns should contain theSameElementsAs (List(IRILabel(y)))
      )
    }

    it(s"Should check node constraint with varName declaration") {
      val x = IRI(s"http://example.org/x")
      val p = IRI(s"http://example.org/p")
      val y = IRI(s"http://example.org/y")
      val rdf = RDFAsJenaModel.empty.addTriples(Set(RDFTriple(x,p,y))).getOrElse(RDFAsJenaModel.empty)
      val tc = TripleConstraint(None,None,None,p,None,None,None,Some(VarName("v")),None,None)
      val c:Check[(ShapeTyping,VarTable)] = checkNodeTripleConstraint(x,tc)
      runCheck(rdf,Schema.empty,c).fold(e => fail(s"Error $e"),
        pair => pair._2.get(VarName("v")) should contain theSameElementsAs (List(y))
      )
    }

  }
}
