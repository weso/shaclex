package es.weso.server

import java.util.concurrent.Executors

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.{DataFormats, Schemas, ValidationTrigger}
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.http4s.dsl.{QueryParamDecoderMatcher, _}
import org.http4s.websocket.WebsocketBits._
import org.http4s.{EntityEncoder, HttpService, LanguageTag, Status}
import org.http4s.server.staticcontent
import org.http4s.server.staticcontent.ResourceService.Config
import org.http4s.server.websocket.WS
import org.http4s.headers._
import org.http4s.circe._
import org.http4s.MediaType._
import org.http4s.twirl._

import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scalaz.stream.{Exchange, Process, time}
import scalaz.stream.async.topic
import es.weso._
import es.weso.rdf.PrefixMap
import org.log4s.getLogger

class Routes {

  private val logger = getLogger
  val API = "api"

//  private implicit val scheduledEC = Executors.newScheduledThreadPool(4)

  // Get the static content
  private val static  = cachedResource(Config("/static", "/static"))
  private val views   = cachedResource(Config("/staticviews", "/"))
  private val swagger = cachedResource(Config("/swagger", "/swagger"))

  object DataParam extends QueryParamDecoderMatcher[String]("data")
  object DataFormatParam extends OptionalQueryParamDecoderMatcher[String]("dataFormat")
  object ResultDataFormatParam extends OptionalQueryParamDecoderMatcher[String]("resultFormat")
  object SchemaParam extends OptionalQueryParamDecoderMatcher[String]("schema")
  object SchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("schemaFormat")
  object SchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("schemaEngine")
  object ResultSchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("resultFormat")
  object ResultSchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("resultEngine")
  object TriggerModeParam extends OptionalQueryParamDecoderMatcher[String]("triggerMode")
  object NodeParam extends OptionalQueryParamDecoderMatcher[String]("node")
  object ShapeParam extends OptionalQueryParamDecoderMatcher[String]("shape")
  object NameParam extends OptionalQueryParamDecoderMatcher[String]("name")

  val service: HttpService = HttpService {

    // Web site
    case req @ GET -> Root / "dataOptions" => {
      Ok(html.dataOptions(DataFormats.formatNames.toList, DataFormats.defaultFormatName))
    }

    case req @ GET -> Root / "schemaOptions" => {
      Ok(html.schemaOptions(
        Schemas.availableFormats,
        Schemas.defaultSchemaFormat,
        Schemas.availableSchemaNames,
        Schemas.defaultSchemaName,
        Schemas.availableTriggerModes,
        Schemas.defaultTriggerMode
      ))
    }

    // API methods

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

    case req @ GET -> Root / API / "data" / "info" :?
      DataParam(data) +&
      DataFormatParam(optDataFormat) => {
      val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
      RDFAsJenaModel.fromChars(data,dataFormat,None) match {
        case Failure(e) => BadRequest(s"Error reading rdf: $e\nRdf string: $data")
        case Success(rdf) => {
          val nodes: List[String] =
          (
            rdf.subjects() ++
            rdf.objects() ++
            rdf.predicates()).map(_.toString).toList
          val jsonNodes : Json = Json.fromValues(nodes.map(str => Json.fromString(str)))
          val pm: Json = prefixMap2Json(rdf.getPrefixMap)
          val result = DataInfoResult(data,dataFormat,jsonNodes,pm).asJson
          Ok(result).
            withContentType(Some(`Content-Type`(`application/json`))).
            withStatus(Status.Ok)
        }
      }
    }

    case req @ GET -> Root / API / "schema" / "info" :?
      SchemaParam(optSchema) +&
      SchemaFormatParam(optSchemaFormat) +&
      SchemaEngineParam(optSchemaEngine) => {
      val schemaEngine = optSchemaEngine.getOrElse(Schemas.defaultSchemaName)
      val schemaFormat = optSchemaFormat.getOrElse(Schemas.defaultSchemaFormat)
      val schemaStr = optSchema match {
        case None => ""
        case Some(schema) => schema
      }
      Schemas.fromString(schemaStr,schemaFormat,schemaEngine,None) match {
        case Failure(e) => BadRequest(s"Error reading schema: $e\nString: $schemaStr")
        case Success(schema) => {
          val shapes: List[String] = schema.shapes
          val jsonShapes = Json.fromValues(shapes.map(Json.fromString(_)))
          val pm: Json = prefixMap2Json(schema.pm)
//          implicit val encoder: EntityEncoder[SchemaInfoResult] = ???
          val result = SchemaInfoResult(schemaStr,schemaFormat,schemaEngine,jsonShapes, pm).asJson
          Ok(result).
            withContentType(Some(`Content-Type`(`application/json`))).
            withStatus(Status.Ok)
        }
      }
    }

    case req @ GET -> Root / API / "data" / "convert" :?
      DataParam(data) +&
      DataFormatParam(optDataFormat) +&
      ResultDataFormatParam(optResultDataFormat) => {
      val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
      val resultDataFormat = optResultDataFormat.getOrElse(DataFormats.defaultFormatName)

      RDFAsJenaModel.fromChars(data,dataFormat,None) match {
        case Failure(e) => BadRequest(s"Error reading RDF Data: $e\nString: $data")
        case Success(rdf) => {
            val resultStr = rdf.serialize(resultDataFormat)
            val result = DataConversionResult(data,dataFormat,resultDataFormat,resultStr)
            val default = Ok(result.asJson)
              .withContentType(Some(`Content-Type`(`application/json`)))
            req.headers.get(`Accept`) match {
              case Some(ah) => {
                logger.info(s"Accept header: $ah")
                val hasHTML : Boolean = ah.values.exists(mr => mr.mediaRange.satisfiedBy(`text/html`))
                if (hasHTML) {
                  Ok(result.toHTML).withContentType(Some(`Content-Type`(`text/html`)))
                } else default
              }
              case None => default
            }
        }
      }
    }

    case req @ GET -> Root / API / "schema" / "convert" :?
      SchemaParam(optSchema) +&
        SchemaFormatParam(optSchemaFormat) +&
        SchemaEngineParam(optSchemaEngine) +&
        ResultSchemaFormatParam(optResultSchemaFormat) +&
        ResultSchemaEngineParam(optResultSchemaEngine) => {
      val schemaEngine = optSchemaEngine.getOrElse(Schemas.defaultSchemaName)
      val schemaFormat = optSchemaFormat.getOrElse(Schemas.defaultSchemaFormat)
      val resultSchemaFormat = optResultSchemaFormat.getOrElse(Schemas.defaultSchemaFormat)
      val resultSchemaEngine = optResultSchemaEngine.getOrElse(Schemas.defaultSchemaName)

      val schemaStr = optSchema match {
        case None => ""
        case Some(schema) => schema
      }
      Schemas.fromString(schemaStr,schemaFormat,schemaEngine,None) match {
        case Failure(e) => BadRequest(s"Error reading schema: $e\nString: $schemaStr")
        case Success(schema) => {
          if (schemaEngine.toUpperCase == resultSchemaEngine.toUpperCase) {
            schema.serialize(resultSchemaFormat) match {
              case Success(resultStr) => {
                val result = SchemaConversionResult(schemaStr,schemaFormat,schemaEngine,
                  resultSchemaFormat,resultSchemaEngine,resultStr)
                val default = Ok(result.asJson)
                  .withContentType(Some(`Content-Type`(`application/json`)))
                req.headers.get(`Accept`) match {
                  case Some(ah) => {
                    logger.info(s"Accept header: $ah")
                    val hasHTML : Boolean = ah.values.exists(mr => mr.mediaRange.satisfiedBy(`text/html`))
                    if (hasHTML) {
                      Ok(result.toHTML).withContentType(Some(`Content-Type`(`text/html`)))
                    } else default
                  }
                  case None => default
                }
              }
              case Failure(e) =>
                BadRequest(s"Error serializing $schemaStr with $resultSchemaFormat/$resultSchemaEngine: $e")
            }
          } else {
            BadRequest(s"Conversion between different schema engines not implemented yet: $schemaEngine/$resultSchemaEngine")
          }
        }
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
              Ok(validateResult)
                .withContentType(Some(`Content-Type`(`application/json`)))
                .withStatus(Status.Ok)
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


  def prefixMap2Json(pm: PrefixMap): Json = {
    Json.fromFields(pm.pm.map{ case (prefix,iri) => (prefix.str, Json.fromString(iri.getLexicalForm))})
  }

}