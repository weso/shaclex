package es.weso.server

import cats.effect.IO
import io.circe.Json
import io.circe.parser._
import org.http4s.{ Request, Response, Uri }
import org.http4s.{ Query => HQuery }
import org.scalatest._
import org.http4s.dsl.io._

class ValidateShEx extends FunSpec with Matchers with EitherValues {

  val ip = "0.0.0.0"
  val port = 8080
  val server = new ShaclexServer(ip, port)

  def serve(req: Request[IO]): Response[IO] =
    server.service.orNotFound(req).unsafeRunSync

  describe("ValidateShEx") {
    it("Should return 200 when asking for root") {
      val response = serve(Request(GET, Uri(path = "/")))
      response.status should be(Ok)
    }

    it("Should run test API method") {
      val response = serve(Request(
        GET,
        Uri(
          path = "/api/test",
          query = HQuery.fromPairs(("name", "<John>")))))
      response.status should be(Ok)
      response.as[String].unsafeRunSync() should be("Hello <John>")
    }

    it("Should validate a single example using ShEx") {
      val dataStr =
        """prefix : <http://example.org/>
          |prefix sh: <http://www.w3.org/ns/shacl#>
          |:x :p 1 .
          |:S sh:targetNode :x .
          |""".stripMargin

      val schemaStr =
        """prefix : <http://example.org/>
          |:S { :p . }
          |""".stripMargin

      val response = serve(Request(
        GET,
        Uri(
          path = "/api/validate",
          query = HQuery.fromPairs(
            ("data", dataStr), ("schema", schemaStr),
            ("schemaFormat", "SHEXC"), ("schemaEngine", "ShEx")))))

      response.status should be(Ok)
      val strResponse = response.as[String].unsafeRunSync()
      println(s"strResponse=$strResponse")
      val jsonResponse = parse(strResponse).getOrElse(Json.Null)
      println(s"jsonResponse=$jsonResponse")
      val isValid: Option[Boolean] =
        jsonResponse.hcursor.get[Boolean]("valid").toOption
      isValid shouldBe Some(true)
    }
  }
}
