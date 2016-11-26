package es.weso.server

import java.util.concurrent.Executors

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.{DataFormats, Schemas}
import org.http4s.dsl._
import org.http4s.websocket.WebsocketBits._
import org.http4s.HttpService
import org.http4s.server.staticcontent
import org.http4s.server.staticcontent.ResourceService.Config
import org.http4s.server.websocket.WS

import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scalaz.stream.{Exchange, Process, time}
import scalaz.stream.async.topic

class Routes {
  val API = "api/v1"

  private implicit val scheduledEC = Executors.newScheduledThreadPool(4)

  // Provides the message board for our websocket chat
  private val chatTopic = topic[String]()

  // Get the static content
  private val static  = cachedResource(Config("/static", "/static"))
  private val views   = cachedResource(Config("/staticviews", "/"))
  private val swagger = cachedResource(Config("/swagger", "/swagger"))

  object DataParam extends QueryParamDecoderMatcher[String]("data")
  object DataFormatParam extends OptionalQueryParamDecoderMatcher[String]("dataFormat")
  object SchemaParam extends QueryParamDecoderMatcher[String]("schema")
  object SchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("schemaFormat")
  object SchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("schemaEngine")

  val service: HttpService = HttpService {

    case request @ ( GET | POST) -> Root / API / "validate" :?
      DataParam(data) +&
        DataFormatParam(optDataFormat) +&
        SchemaParam(schema) +&
        SchemaFormatParam(optSchemaFormat) +&
        SchemaEngineParam(optSchemaEngine) => {
      val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
      val schemaFormat = optSchemaFormat.getOrElse(Schemas.defaultSchemaFormat)
      val schemaEngine = optSchemaEngine.getOrElse(Schemas.defaultSchemaName)

      Schemas.fromString(schema,schemaFormat,schemaEngine,None) match {
        case Failure(e) => BadRequest(s"Error reading schema: $e\nString: $schema")
        case Success(schema) => {
          RDFAsJenaModel.fromChars(data,dataFormat,None) match {
            case Failure(e) => BadRequest(s"Error reading rdf: $e\nRdf string: $data")
            case Success(rdf) => {
              val result = schema.validate(rdf)
              val jsonResult = result.toJsonString2spaces
              Ok(jsonResult)
            }
          }
        }
      }
  }

  case r @ GET -> _ if r.pathInfo.startsWith("/static") => static(r)

    case r @ GET -> _ if r.pathInfo.startsWith("/swagger") => swagger(r)

    case r @ GET -> _ if r.pathInfo.endsWith("/") =>
      service(r.withPathInfo(r.pathInfo + "index.html"))

    case r @ GET -> _ =>
      val rr = if (r.pathInfo.contains('.')) r else r.withPathInfo(r.pathInfo + ".html")
      views(rr)
  }

  private def cachedResource(config: Config): HttpService = {
    val cachedConfig = config.copy(cacheStartegy = staticcontent.MemoryCache())
    staticcontent.resourceService(cachedConfig)
  }
}