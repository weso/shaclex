package es.weso.shex.linter

import es.weso._
import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.utils.IOUtils._
import org.scalatest.funspec._
import org.scalatest.matchers.should._

class CountUsagesTest extends AnyFunSpec with Matchers {

 describe(s"Count usages") {

   shouldCount(
     """|prefix : <>
        |<S> { :p . }
     """.stripMargin,
     Map(IRILabel(IRI("S")) -> 0)
   )

   shouldCount("""|prefix : <http://example.org/>
                  |prefix sh: <http://www.w3.org/ns/shacl#>
                  |:S { &:PS }
                  |
                  |_:1 {
                  | $:PS :p IRI
                  |}
               """.stripMargin, Map(IRILabel(IRI("http://example.org/S")) -> 0,
                                BNodeLabel(BNode("1")) -> 0
   ))

   shouldCount("""|prefix : <http://example.org/>
                  |prefix sh: <http://www.w3.org/ns/shacl#>
                  |:S { &:PS }
                  |:T { &:PS }
                  |
                  |_:1 {
                  | $:PS :p IRI
                  |}
               """.stripMargin, Map(IRILabel(IRI("http://example.org/S")) -> 0,
                                    IRILabel(IRI("http://example.org/T")) -> 0,
                                    BNodeLabel(BNode("1")) -> 0))

   shouldCount("""|prefix : <http://example.org/>
                  |prefix sh: <http://www.w3.org/ns/shacl#>
                  |:S { &_:1 }
                  |
                  |_:1 {
                  | :p IRI
                  |}
               """.stripMargin, Map(IRILabel(IRI("http://example.org/S")) -> 0,
                                    BNodeLabel(BNode("1")) -> 1
                                   )
                )

   shouldCount("""|prefix : <http://example.org/>
                  |prefix sh: <http://www.w3.org/ns/shacl#>
                  |:S { &_:1 }
                  |:T { &_:1 }
                  |
                  |_:1 {
                  | :p IRI
                  |}
               """.stripMargin, Map(IRILabel(IRI("http://example.org/S")) -> 0,
                                    BNodeLabel(BNode("1")) -> 2,
                                    IRILabel(IRI("http://example.org/T")) -> 0,
                                   )
               )
 }

 def shouldCount(shexStr: String,
                 mapExpected: Map[ShapeLabel,Int]
                ): Unit = {
   it(s"Should count usages $shexStr and obtain $mapExpected") {
   val r = for {
     schema <- io2es(shex.Schema.fromString(shexStr,"SHEXC"))
     result <- either2es(CountUsages.countUsages(schema))
   } yield (schema,result)

   r.value.unsafeRunSync.fold(e => fail(s"Error: $e"), values => {
     val (_,result) = values
     pprint.log(result, "Result")
     result.toList should contain theSameElementsAs(mapExpected.toList)
   })
  }
 }
}
