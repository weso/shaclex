package es.weso.schema

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import org.scalatest._

class InfoNodeTest extends FunSpec with Matchers with EitherValues with JsonTest {

  describe("Solution") {
    encodeDecodeTest(InfoNode(
        hasShapes = Seq((SchemaLabel("S"), Explanation("ES"))),
        hasNoShapes = Seq((SchemaLabel("T"), Explanation("ET"))),
        pm = PrefixMap.empty
      ))
    encodeDecodeTest(InfoNode(
      hasShapes = Seq((SchemaLabel("S"), Explanation("ES")),
        (SchemaLabel("T"), Explanation("ET"))),
      hasNoShapes = Seq(),
      pm = PrefixMap.empty
    ))

  }
}
