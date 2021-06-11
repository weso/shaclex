package es.weso.shex.linter

import cats._
import es.weso._
import es.weso.shex.Schema
import es.weso.shex.implicits.eqShEx._
import es.weso.utils.IOUtils._
import munit._

class ShExLinterTest extends CatsEffectSuite {

/*   shouldSimplify(
     """|prefix : <>
        |<S> { :p . }
     """.stripMargin,
     """|prefix : <>
        |<S> { :p . }
     """.stripMargin) */

   shouldSimplify(
     """|prefix : <>
        |<S> { &_:1 }
        |_:1 { :p . }
     """.stripMargin,
     """|prefix : <>
        |<S> { :p . }
     """.stripMargin)

 def shouldSimplify(shexStr: String,
                    strExpected: String
                   ): Unit = {
   test(s"Should simplify $shexStr and obtain $strExpected") {
   val r = for {
     schema <- io2es(shex.Schema.fromString(shexStr,"SHEXC"))
     simplified <- either2es(ShExLinter.inlineInclusions(schema))
     expected <- io2es(shex.Schema.fromString(strExpected,"SHEXC"))
   } yield (schema,simplified, expected)

   run_es(r).unsafeRunSync.fold(e => fail(s"Error: $e"), values => {
     val (schema,simplified,expected) = values
     if (Eq[Schema].eqv(simplified,expected)) {
       assertEquals(true,true)
     } else {
       fail(s"Schemas are different: \nObtained: \n$schema\nExpected:\n$expected")
     }
   })

  }
 }
}
