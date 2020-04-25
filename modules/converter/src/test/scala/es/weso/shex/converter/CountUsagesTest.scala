package es.weso.shex.converter

import es.weso._
import es.weso.rdf.nodes._
import es.weso.shex._
import org.scalatest.funspec._
import org.scalatest.matchers.should._
import es.weso.utils.IOUtils._

class CountUsagesTest extends AnyFunSpec with Matchers {

 describe(s"Count usages") {
   shouldCount(
     """|prefix : <>
        |<S> { :p . }
     """.stripMargin,
     Map()
   )

   shouldCount("""|prefix : <http://example.org/>
                  |prefix sh: <http://www.w3.org/ns/shacl#>
                  |:S { &:PS }
                  |
                  |_:1 {
                  | $:PS :p IRI
                  |}
               """.stripMargin, Map(IRILabel(IRI("http://example.org/PS")) -> 1))
 }

 def shouldCount(shexStr: String,
                 mapExpected: Map[ShapeLabel,Int]
                ): Unit = {
   it(s"Should count usages $shexStr and obtain $mapExpected") {
   val r = for {
     schema <- io2es(shex.Schema.fromString(shexStr,"SHEXC"))
     result <- either2es(CountUsages.countUsages(schema))
   } yield (schema,result)

   r.fold(e => fail(s"Error: $e"), values => {
     val (schema,result) = values
     result.toList should contain theSameElementsAs(mapExpected.toList)
   })
  }
 }
}
