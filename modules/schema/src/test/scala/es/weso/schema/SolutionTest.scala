package es.weso.schema

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{IRI, RDFNode}
import org.scalatest._

import scala.util._

class SolutionTest extends FunSpec with Matchers with EitherValues with JsonTest {

  describe("Solution") {
      encodeDecodeTest(Solution(
        nodes = Map(IRI("http://example.org") -> InfoNode(
          hasShapes = Seq((SchemaLabel("S"), Explanation(""))),
          hasNoShapes = Seq(),
          pm = PrefixMap.empty
        )),
        nodeMap = PrefixMap.empty,
        schemaMap = PrefixMap.empty
      ))
  }
}
