package es.weso.utils.test
import io.circe.Json
import org.scalatest._

class JsonMatchersTest extends FunSpec with Matchers with JsonMatchers {

  describe("Simple Json test") {
    it(s"Should check two json values") {
      val json1 =
        """|{ "key1": 24 }
           |""".stripMargin
      val expected1 =
        """|{ "key1": 24 }
           |""".stripMargin
     json1 should matchJson(expected1)
    }

    it(s"Should check two different json values dont match") {
      val json1 =
        """|{ "key1": 24 }
           |""".stripMargin
      val expected1 =
        """|{ "key1": 23 }
           |""".stripMargin
      json1 shouldNot matchJson(expected1)
    }

    it(s"Should check a Json with another Json values dont match") {
      val json = Json.fromFields(List(
        ("key1",Json.fromString("value1")),
        ("key2",Json.fromString("value2"))
        ))
      val expected: Json = Json.fromFields(List(
        ("key2",Json.fromString("value2")),
        ("key1",Json.fromString("value1"))
      ))
      json should be (expected)
    }

  }


}