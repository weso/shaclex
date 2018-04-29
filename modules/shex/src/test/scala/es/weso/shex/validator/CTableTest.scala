package es.weso.shex.validator

import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.shex.validator.table.CTable
import org.scalatest._

class CTableTest extends FunSpec with Matchers with EitherValues {
  describe(s"CTable") {
    it(s"Should generate CTable") {
      val ex = "http://example.org/"
      val xsd = IRI(s"http://www.w3.org/2001/XMLSchema#")
      val p = IRI(ex) + "p"
      val q = IRI(ex) + "q"
      val xsd_string = xsd + "string"
      val te1: TripleExpr = TripleConstraint.datatype(p,xsd_string, List())
      val te2: TripleExpr = TripleConstraint.datatype(q,xsd_string, List())
      val te : TripleExpr = EachOf(None, List(te1,te2),None,None,None,None)
      val extras: List[IRI] = List()
      val s: ShapeLabel = IRILabel(IRI(ex + "S"))
      val tripleExprMap : Map[ShapeLabel, TripleExpr] = Map(s -> te)
      val maybeTable = CTable.mkTable(te,extras,tripleExprMap)

      maybeTable.fold(e => fail(e),result => {
        val rbe = result._2
        val c0 = ConstraintRef(0)
        val c1 = ConstraintRef(1)
        rbe.symbols should contain only(c0,c1)

        val ctable = result._1
        info(s"${ctable}")

       })

    }
  }
}
