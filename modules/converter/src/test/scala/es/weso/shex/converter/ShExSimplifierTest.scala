package es.weso.shex.converter

import cats._
import es.weso._
import es.weso.shex.implicits.eqShEx._
import es.weso.shex.Schema
import org.scalatest._

class ShExSimplifierTest extends FunSpec
  with Matchers
  with EitherValues
  with ShExSimplifier {

 describe(s"ShExSimplifier") {
   shouldSimplify(
     """|prefix : <>
        |<S> { :p . }
     """.stripMargin,
     """|prefix : <>
        |<S> { :p . }
     """.stripMargin)
 }

 def shouldSimplify(shexStr: String,
                    strExpected: String
                   ): Unit = {
   it(s"Should simplify $shexStr and obtain $strExpected") {
   val r = for {
     schema <- shex.Schema.fromString(shexStr,"SHEXC")
     simplified <- inlineInclusions(schema)
     expected <- shex.Schema.fromString(strExpected,"SHEXC")

   } yield (schema,simplified, expected)

   r.fold(e => fail(s"Error: $e"), values => {
     val (schema,simplified,expected) = values
     if (Eq[Schema].eqv(simplified,expected)) {
       info(s"Schemas are equal")
     } else {
       fail(s"Schemas are different: \nObtained: \n$schema\nExpected:\n$expected")
     }
   })

  }
 }
}
