package es.weso.server

import io.circe.Json
import io.circe.parser._
import org.http4s.{ Query, Request, Response, Uri }
import org.scalatest._
import org.http4s.dsl._
import cats.syntax.either._

class ValidateShEx extends FunSpec with Matchers with EitherValues {

  val ip = "0.0.0.0"
  val port = 8080
  val server = new ShaclexServer(ip, port)

  def serve(req: Request): Response =
    server.service.run(req).unsafeRun().orNotFound

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
          query = Query.fromPairs(("name", "<John>")))))
      response.status should be(Ok)
      response.as[String].unsafeRun() should be("Hello <John>")
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
          query = Query.fromPairs(
            ("data", dataStr), ("schema", schemaStr),
            ("schemaFormat", "SHEXC"), ("schemaEngine", "ShEx")))))

      response.status should be(Ok)
      val strResponse = response.as[String].unsafeRun()
      val jsonResponse = parse(strResponse).getOrElse(Json.Null)
      val isValid: Option[Boolean] =
        jsonResponse.hcursor.get[Boolean]("isValid").toOption
      isValid shouldBe Some(true)
    }
  }
}
