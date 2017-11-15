package es.weso.server

import java.util.concurrent.Executors

import cats.effect.IO
import org.http4s.util.{ExitCode, StreamApp}
import org.http4s._
import org.http4s.server.blaze._
import org.http4s.server.middleware.CORS
import org.http4s.server._
import org.log4s.getLogger
import fs2.Stream

import scala.util.Properties.envOrNone
import cats._
import cats.data._
import cats.implicits._

class ShaclexServer(host: String, port: Int) {
  private val logger = getLogger
  private val pool = Executors.newCachedThreadPool()

  logger.info(s"Starting Http4s-blaze example on '$host:$port'")

  val routesService: HttpService[IO] =
    CORS(APIService.apiService <+> WebService.webService)

  val service: HttpService[IO] = routesService.local { req =>
    val path = req.uri.path
    logger.info(s"Request with path: ${req.remoteAddr.getOrElse("null")} -> ${req.method}: $path")
    req
  }

  def build(): Stream[IO, ExitCode] =
    BlazeBuilder[IO]
      .bindHttp(port, host)
      .mountService(service).
      serve
}

object ShaclexServer extends StreamApp[IO] {
  val ip = "0.0.0.0"
  val port = envOrNone("PORT") map (_.toInt) getOrElse (8080)

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    new ShaclexServer(ip, port).build()
  }

}