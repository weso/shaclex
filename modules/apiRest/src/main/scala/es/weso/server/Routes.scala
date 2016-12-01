package es.weso.server

import java.util.concurrent.Executors

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.{DataFormats, Schemas, ValidationTrigger}
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import org.http4s.dsl.{QueryParamDecoderMatcher, _}
import org.http4s.websocket.WebsocketBits._
import org.http4s.{EntityEncoder, HttpService, LanguageTag}
import org.http4s.server.staticcontent
import org.http4s.server.staticcontent.ResourceService.Config
import org.http4s.server.websocket.WS
import org.http4s.headers._
import org.http4s.circe._
import org.http4s.MediaType._

import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scalaz.stream.{Exchange, Process, time}
import scalaz.stream.async.topic

class Routes {
  val API = "api"

//  private implicit val scheduledEC = Executors.newScheduledThreadPool(4)

  // Get the static content
  private val static  = cachedResource(Config("/static", "/static"))
  private val views   = cachedResource(Config("/staticviews", "/"))
  private val swagger = cachedResource(Config("/swagger", "/swagger"))

  object DataParam extends QueryParamDecoderMatcher[String]("data")
  object DataFormatParam extends OptionalQueryParamDecoderMatcher[String]("dataFormat")
  object SchemaParam extends OptionalQueryParamDecoderMatcher[String]("schema")
  object SchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("schemaFormat")
  object SchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("schemaEngine")
  object TriggerModeParam extends OptionalQueryParamDecoderMatcher[String]("triggerMode")
  object NodeParam extends OptionalQueryParamDecoderMatcher[String]("node")
  object ShapeParam extends OptionalQueryParamDecoderMatcher[String]("shape")
  object NameParam extends OptionalQueryParamDecoderMatcher[String]("name")

  val service: HttpService = HttpService {

    case GET -> Root / API / "schema" / "engines" => {
      val engines = Schemas.availableSchemaNames
      val json = Json.fromValues(engines.map(str => Json.fromString(str)))
      Ok(json)
    }

    case GET ->  Root / API / "schema" / "engines" / "default" => {
      val schemaEngine = Schemas.defaultSchemaName
      Ok(schemaEngine)
    }

    case  GET -> Root / API / "schema" / "formats" => {
        val formats = Schemas.availableFormats
        val json = Json.fromValues(formats.map(str => Json.fromString(str)))
        Ok(json)
      }

     case  GET -> Root / API / "schema" / "triggerModes" => {
        val triggerModes = ValidationTrigger.triggerValues.map(_._1)
        val json = Json.fromValues(triggerModes.map(Json.fromString(_)))
        Ok(json)
      }

    case GET -> Root / API / "data" / "formats" => {
        val formats = DataFormats.formatNames
        val json = Json.fromValues(formats.map(str => Json.fromString(str)))
        Ok(json)
      }

    case GET -> Root / API / "schema" / "engines" / "default" => {
        val schemaEngine = Schemas.defaultSchemaName
        Ok(schemaEngine)
      }

    case req @ GET -> Root / API / "test" :? NameParam(name) => {
      val default = Ok(s"Hello ${name.getOrElse("World")}")
      req.headers.get(`Accept-Language`) match {
        case Some(al) => {
          al match {
            case _ if (al.satisfiedBy(LanguageTag("es"))) =>
              Ok(s"Hola ${name.getOrElse("Mundo")}!")
            case _ => default
          }
        }
        case None => default
      }
    }

    case request @ ( GET | POST) -> Root / API / "validate" :?
      DataParam(data) +&
      DataFormatParam(optDataFormat) +&
      SchemaParam(optSchema) +&
      SchemaFormatParam(optSchemaFormat) +&
      SchemaEngineParam(optSchemaEngine) +&
      TriggerModeParam(optTriggerMode) +&
      NodeParam(optNode) +&
      ShapeParam(optShape) => {
      val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
      val schemaEngine = optSchemaEngine.getOrElse(Schemas.defaultSchemaName)
      val schemaFormat = optSchema match {
        case None => dataFormat
        case Some(_) => optSchemaFormat.getOrElse(Schemas.defaultSchemaFormat)
      }
      val schemaStr = optSchema match {
        case None => data
        case Some(schema) => schema
      }
      val triggerMode = optTriggerMode.getOrElse(ValidationTrigger.default.name)

      Schemas.fromString(schemaStr,schemaFormat,schemaEngine,None) match {
        case Failure(e) => BadRequest(s"Error reading schema: $e\nString: $schemaStr")
        case Success(schema) => {
          RDFAsJenaModel.fromChars(data,dataFormat,None) match {
            case Failure(e) => BadRequest(s"Error reading rdf: $e\nRdf string: $data")
            case Success(rdf) => {
              val result = schema.validate(rdf,triggerMode,optNode,optShape)
              val jsonResult = result.toJson
              val validateResult =
                ValidateResult(data,dataFormat,
                  schemaStr,schemaFormat,schemaEngine,
                  triggerMode,optNode,optShape,jsonResult).asJson
              Ok(validateResult).withContentType(Some(`Content-Type`(`application/json`)))
            }
          }
        }
      }
  }

  // Contents on /static are mapped to /static
  case r @ GET -> _ if r.pathInfo.startsWith("/static") => static(r)

  // Contents on /swagger are directly mapped to /swagger
  case r @ GET -> _ if r.pathInfo.startsWith("/swagger/") => swagger(r)

  // case r @ GET -> _ if r.pathInfo.startsWith("/swagger.json") => views(r)

  // When accessing to a folder (ends by /) append index.html
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


  case class VR(name: String)
}