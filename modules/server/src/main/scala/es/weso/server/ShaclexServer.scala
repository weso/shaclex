package es.weso.server

// import java.util.concurrent.Executors

// import org.http4s.util.{ExitCode, StreamApp}
import fs2.StreamApp
import fs2.StreamApp.ExitCode
import scala.concurrent.ExecutionContext.Implicits.global
import org.http4s._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.middleware.CORS
import org.log4s.getLogger
// import fs2.Stream
import scala.util.Properties.envOrNone
// import cats._
// import cats.data._
import cats.implicits._
import cats.effect.IO

class ShaclexServer(host: String, port: Int) {
  private val logger = getLogger
  // private val pool = Executors.newCachedThreadPool()

  logger.info(s"Starting Http4s-blaze example on '$host:$port'")

  val routesService: HttpService[IO] =
    CORS(APIService.apiService <+> WebService.webService)

  val service: HttpService[IO] = routesService.local { req =>
    val path = req.uri.path
    logger.info(s"Request with path: ${req.remoteAddr.getOrElse("null")} -> ${req.method}: $path")
    req
  }

  def build(): fs2.Stream[IO, ExitCode] =
    BlazeBuilder[IO]
      .bindHttp(port, host)
      .mountService(service).
      serve
}

object ShaclexServer extends StreamApp[IO] {
  val ip = "0.0.0.0"
  val port = envOrNone("PORT") map (_.toInt) getOrElse (8080)

  override def stream(args: List[String], 
                      requestShutdown: IO[Unit]) = {
    println(s"Before starting ShaclexServer on port $port and IP $ip")
    new ShaclexServer(ip, port).build()
  }

}