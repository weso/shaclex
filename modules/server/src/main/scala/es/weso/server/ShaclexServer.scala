package es.weso.server

import java.util.concurrent.Executors

import cats.effect.IO
import org.http4s.util.StreamApp

// import fs2.Task
import org.http4s._
//import org.http4s.rho.swagger.SwaggerSupport
//import org.http4s.rho.swagger.models.Info
import org.http4s.server.blaze._
import org.http4s.server.middleware.CORS
import org.http4s.server.{ Server, ServerApp, ServerBuilder }
import org.log4s.getLogger
import fs2.Stream
import scala.util.Properties.envOrNone
// import scalaz.concurrent.Task

class ShaclexServer(host: String, port: Int) {
  private val logger = getLogger
  private val pool = Executors.newCachedThreadPool()

  logger.info(s"Starting Http4s-blaze example on '$host:$port'")

  val routes = CORS(new Routes().service)

  val service: HttpService[IO] = routes.local { req =>
    logger.info(s"Request: $req")
    val path = req.uri.path
    logger.info(s"Request with path: ${req.remoteAddr.getOrElse("null")} -> ${req.method}: $path")
    req
  }

  def build(): Stream[IO, Nothing] =
    BlazeBuilder[IO]
      .bindHttp(port, host)
      .mountService(service).
      serve
}

object ShaclexServer extends StreamApp[IO] {
  val ip = "0.0.0.0"
  val port = envOrNone("PORT") map (_.toInt) getOrElse (8080)

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, Nothing] = {
    new ShaclexServer(ip, port).build()
  }

}