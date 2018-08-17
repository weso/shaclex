package es.weso.shex.converter

import cats._
import es.weso._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.nodes.BNode
import es.weso.shex.{BNodeLabel, Schema, ShapeLabel}
import es.weso.shex.implicits.eqShEx._
import org.scalatest._

class CountUsagesTest extends FunSpec with Matchers with EitherValues {

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
               """.stripMargin, Map(BNodeLabel(BNode("PS")) -> 1))
 }

 def shouldCount(shexStr: String,
                 mapExpected: Map[ShapeLabel,Int]
                ): Unit = {
   it(s"Should count usages $shexStr and obtain $mapExpected") {
   val r = for {
     schema <- shex.Schema.fromString(shexStr,"SHEXC",None, RDFAsJenaModel.empty)
     result <- CountUsages.countUsages(schema)
   } yield (schema,result)

   r.fold(e => fail(s"Error: $e"), values => {
     val (schema,result) = values
     result.toList should contain theSameElementsAs(mapExpected.toList)
   })
  }
 }
}
