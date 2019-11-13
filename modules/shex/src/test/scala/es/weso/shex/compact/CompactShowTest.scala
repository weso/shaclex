package es.weso.shex.compact

import cats.implicits._
import es.weso.shex.Schema
import org.scalatest._

class CompactShowTest extends FunSpec with Matchers with EitherValues {

  describe(s"Compact show test") {
   shouldShowAndParse(
     """|prefix : <http://example.org/>
       |:S IRI
     """.stripMargin)
  }

  def shouldShowAndParse(shexStr: String): Unit = {
    it(s"Should show/parse $shexStr") {
    val result = for {
      shExSchema <- Schema.fromString(shexStr)
      serialized = CompactShow.showSchema(shExSchema)
      schemaFromSerialized <- Schema
          .fromString(serialized)
          .leftMap(e => s"Error reading serialized schema: $e\nSerialized:\n$serialized")
      b <- shouldBeEqualSchemas(shExSchema, schemaFromSerialized)
    } yield b
    result.fold(e => fail(s"$e"), v => v should be(true))
    }
  }

  def shouldBeEqualSchemas(s1: Schema, s2: Schema): Either[String, Boolean] =
    if (s1 === s2) Right(true)
    else Right(false)
}
