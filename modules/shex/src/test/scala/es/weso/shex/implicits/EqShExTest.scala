package es.weso.shex.implicits

import es.weso.rdf.{Prefix, PrefixMap}
import es.weso.rdf.nodes.IRI
import es.weso.shex._
import org.scalatest._
import cats.implicits._

class EqShExTest extends FunSpec with Matchers with EitherValues {
  describe(s"Eq ShEx Schema") {
    it(s"Should compare single schemas ignoring namespaces") {
      val s1 = Schema(IRI(""),None,None,None,None,None,None,List())
      val pm: PrefixMap = PrefixMap(Map(Prefix("ex") -> IRI("http://example.org/")))
      val s2 = Schema(IRI(""),Some(pm),None,None,None,None,None,List())
      s1 === s2 should be(false)
    }
  }

}
