package es.weso.server

import java.util.concurrent.Executors

import org.http4s._
import org.http4s.rho.swagger.SwaggerSupport
import org.http4s.rho.swagger.models.Info
import org.http4s.server.blaze._
import org.http4s.server.middleware.CORS
import org.http4s.server.{Server, ServerApp, ServerBuilder}
import org.log4s.getLogger

import scala.util.Properties.envOrNone
import scalaz.concurrent.Task

class ShaclexServer(host: String, port: Int) {
  private val logger = getLogger
  private val pool   = Executors.newCachedThreadPool()

  logger.info(s"Starting Http4s-blaze example on '$host:$port'")

  // build our routes
  def rhoRoutes: HttpService = new RhoRoutes().toService(SwaggerSupport(
    apiPath="swagger3.json",
    apiInfo=Info(title="SHACLEX API generated from Rho", version="0.0.1")
  ))

//  val routes = CORS(Service.withFallback(rhoRoutes)(new Routes().service))
  val routes = CORS(new Routes().service)
  val service: HttpService = routes.local { req =>
    val path = req.uri.path
    logger.info(s"${req.remoteAddr.getOrElse("null")} -> ${req.method}: $path")
    req
  }

  def build(): ServerBuilder =
    BlazeBuilder
      .bindHttp(port, host)
      .mountService(service)
      .withServiceExecutor(pool)
}

object ShaclexServer extends ServerApp {
  val ip   = "0.0.0.0"
  val port = envOrNone("PORT") map (_.toInt) getOrElse (8080)

  override def server(args: List[String]): Task[Server] =
    new ShaclexServer(ip, port)
      .build()
      .start

}