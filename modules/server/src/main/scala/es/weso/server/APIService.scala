package es.weso.server

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.{HttpService, _}
import org.http4s.dsl.io._
import org.http4s.client.blaze.PooledHttp1Client
import org.http4s.server.staticcontent.ResourceService.Config

import cats.effect._, org.http4s._
import org.http4s.headers._
import org.http4s.circe._
import org.http4s.MediaType._
import org.log4s.getLogger
import QueryParams._
import Http4sUtils._
import ApiHelper._

object APIService {

  private val logger = getLogger
  val api = "api"

  private val swagger: HttpService[IO] = staticResource(Config("/swagger", "/swagger"))


  val availableDataFormats = DataFormats.formatNames.toList
  val defaultDataFormat = DataFormats.defaultFormatName
  val availableSchemaFormats = Schemas.availableFormats
  val defaultSchemaFormat = Schemas.defaultSchemaFormat
  val availableSchemaEngines = Schemas.availableSchemaNames
  val defaultSchemaEngine = Schemas.defaultSchemaName
  val availableTriggerModes = Schemas.availableTriggerModes
  val defaultTriggerMode = Schemas.defaultTriggerMode
  val defaultSchemaEmbedded = false

  val apiService: HttpService[IO] = HttpService[IO] {

    case GET -> Root / `api` / "schema" / "engines" => {
      val engines = Schemas.availableSchemaNames
      val json = Json.fromValues(engines.map(str => Json.fromString(str)))
      Ok(json)
    }

    case GET -> Root / `api` / "schema" / "engines" / "default" => {
      val schemaEngine = Schemas.defaultSchemaName
      val json = Json.fromString(schemaEngine)
      Ok(json)
    }

    case GET -> Root / `api` / "schema" / "formats" => {
      val formats = Schemas.availableFormats
      val json = Json.fromValues(formats.map(str => Json.fromString(str)))
      Ok(json)
    }

    case GET -> Root / `api` / "schema" / "triggerModes" => {
      val triggerModes = ValidationTrigger.triggerValues.map(_._1)
      val json = Json.fromValues(triggerModes.map(Json.fromString(_)))
      Ok(json)
    }

    case GET -> Root / `api` / "data" / "formats" => {
      val formats = DataFormats.formatNames
      val json = Json.fromValues(formats.map(str => Json.fromString(str)))
      Ok(json)
    }

    case GET -> Root / `api` / "schema" / "engines" / "default" => {
      val schemaEngine = Schemas.defaultSchemaName
      Ok(Json.fromString(schemaEngine))
    }

    case req @ GET -> Root / `api` / "test" :? NameParam(name) => {
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

    case req @ GET -> Root / `api` / "dataUrl" / "info" :?
      OptDataURLParam(optDataUrl) +&
      DataFormatParam(optDataFormat) => {
      val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
      val httpClient = PooledHttp1Client[IO]()
      optDataUrl match {
        case None => BadRequest(s"Must provide a dataUrl")
        case Some(dataUrl) => {
          httpClient.expect[String](dataUrl).flatMap(data => {
            RDFAsJenaModel.fromChars(data, dataFormat, None) match {
              case Left(e) => BadRequest(s"Error reading rdf: $e\nRdf string: $data")
              case Right(rdf) => {
                val nodes: List[String] =
                  (
                    rdf.subjects() ++
                      rdf.iriObjects() ++
                      rdf.predicates()).map(_.toString).toList
                val jsonNodes: Json = Json.fromValues(nodes.map(str => Json.fromString(str)))
                val pm: Json = prefixMap2Json(rdf.getPrefixMap)
                val result = DataInfoResult(data, dataFormat, jsonNodes, pm).asJson
                Ok(result).map(_.withContentType(`Content-Type`(`application/json`)))
              }
          }
          })
        }
      }
/*
      } */
    }

    case req @ GET -> Root / `api` / "data" / "info" :?
      DataParameter(data) +&
      DataFormatParam(optDataFormat) => {
      val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
      RDFAsJenaModel.fromChars(data, dataFormat, None) match {
        case Left(e) => BadRequest(s"Error reading rdf: $e\nRdf string: $data")
        case Right(rdf) => {
          val nodes: List[String] =
            (
              rdf.subjects() ++
              rdf.iriObjects() ++
              rdf.predicates()).map(_.toString).toList
          val jsonNodes: Json = Json.fromValues(nodes.map(str => Json.fromString(str)))
          val pm: Json = prefixMap2Json(rdf.getPrefixMap)
          val result = DataInfoResult(data, dataFormat, jsonNodes, pm).asJson
          Ok(result).map(_.withContentType(`Content-Type`(`application/json`)))
        }
      }
    }

    case req @ GET -> Root / `api` / "schema" / "info" :?
      OptSchemaParam(optSchema) +&
      SchemaFormatParam(optSchemaFormat) +&
      SchemaEngineParam(optSchemaEngine) => {
      val schemaEngine = optSchemaEngine.getOrElse(Schemas.defaultSchemaName)
      val schemaFormat = optSchemaFormat.getOrElse(Schemas.defaultSchemaFormat)
      val schemaStr = optSchema match {
        case None => ""
        case Some(schema) => schema
      }
      Schemas.fromString(schemaStr, schemaFormat, schemaEngine, None) match {
        case Left(e) => BadRequest(s"Error reading schema: $e\nString: $schemaStr")
        case Right(schema) => {
          val shapes: List[String] = schema.shapes
          val jsonShapes = Json.fromValues(shapes.map(Json.fromString(_)))
          val pm: Json = prefixMap2Json(schema.pm)
          //          implicit val encoder: EntityEncoder[SchemaInfoResult] = ???
          val result = SchemaInfoResult(schemaStr, schemaFormat, schemaEngine, jsonShapes, pm).asJson
          Ok(result).map(_.withContentType(`Content-Type`(`application/json`)))
        }
      }
    }

    case req @ GET -> Root / `api` / "data" / "convert" :?
      DataParameter(data) +&
      DataFormatParam(optDataFormat) +&
      TargetDataFormatParam(optResultDataFormat) => {
      val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
      val resultDataFormat = optResultDataFormat.getOrElse(DataFormats.defaultFormatName)

      RDFAsJenaModel.fromChars(data, dataFormat, None) match {
        case Left(e) => BadRequest(s"Error reading RDF Data: $e\nString: $data")
        case Right(rdf) => {
          rdf.serialize(resultDataFormat) match {
            case Left(err) =>  BadRequest(s"Error serializing RDF Data: $err\nString: $data")
            case Right(str) => {
              val result = DataConversionResult(data, dataFormat, resultDataFormat, str)
              val default = Ok(result.asJson)
                .map(_.withContentType(`Content-Type`(`application/json`)))
              req.headers.get(`Accept`) match {
                case Some(ah) => {
                  logger.info(s"Accept header: $ah")
                  val hasHTML: Boolean = ah.values.exists(mr => mr.mediaRange.satisfiedBy(`text/html`))
                  if (hasHTML) {
                    Ok(result.toHTML).map(_.withContentType(`Content-Type`(`text/html`)))
                  } else default
                }
                case None => default
              }
            }
          }
        }
      }
    }

    case req @ GET -> Root / `api` / "schema" / "convert" :?
      OptSchemaParam(optSchema) +&
      SchemaFormatParam(optSchemaFormat) +&
      SchemaEngineParam(optSchemaEngine) +&
      TargetSchemaFormatParam(optResultSchemaFormat) +&
      TargetSchemaEngineParam(optResultSchemaEngine) => {
      val schemaEngine = optSchemaEngine.getOrElse(Schemas.defaultSchemaName)
      val schemaFormat = optSchemaFormat.getOrElse(Schemas.defaultSchemaFormat)
      val resultSchemaFormat = optResultSchemaFormat.getOrElse(Schemas.defaultSchemaFormat)
      val resultSchemaEngine = optResultSchemaEngine.getOrElse(Schemas.defaultSchemaName)

      val schemaStr = optSchema match {
        case None => ""
        case Some(schema) => schema
      }
      Schemas.fromString(schemaStr, schemaFormat, schemaEngine, None) match {
        case Left(e) => BadRequest(s"Error reading schema: $e\nString: $schemaStr")
        case Right(schema) => {
          if (schemaEngine.toUpperCase == resultSchemaEngine.toUpperCase) {
            schema.serialize(resultSchemaFormat) match {
              case Right(resultStr) => {
                val result = SchemaConversionResult(schemaStr, schemaFormat, schemaEngine,
                  resultSchemaFormat, resultSchemaEngine, resultStr)
                val default = Ok(result.asJson)
                  .map(_.withContentType(`Content-Type`(`application/json`)))
                req.headers.get(`Accept`) match {
                  case Some(ah) => {
                    logger.info(s"Accept header: $ah")
                    val hasHTML: Boolean = ah.values.exists(mr => mr.mediaRange.satisfiedBy(`text/html`))
                    if (hasHTML) {
                      Ok(result.toHTML).map(_.withContentType(`Content-Type`(`text/html`)))
                    } else default
                  }
                  case None => default
                }
              }
              case Left(e) =>
                BadRequest(s"Error serializing $schemaStr with $resultSchemaFormat/$resultSchemaEngine: $e")
            }
          } else {
            BadRequest(s"Conversion between different schema engines not implemented yet: $schemaEngine/$resultSchemaEngine")
          }
        }
      }
    }

    case req @ (GET | POST) -> Root / `api` / "validate" :?
      DataParameter(data) +&
      DataFormatParam(optDataFormat) +&
      OptSchemaParam(optSchema) +&
      SchemaFormatParam(optSchemaFormat) +&
      SchemaEngineParam(optSchemaEngine) +&
      OptTriggerModeParam(optTriggerMode) +&
      NodeParam(optNode) +&
      ShapeParam(optShape) +&
      ShapeMapParameter(optShapeMap) +&
      ShapeMapURLParameter(optShapeMapURL) +&
      ShapeMapFileParameter(optShapeMapFile) +&
      ShapeMapFormatParam(optShapeMapFormat) +&
      OptActiveShapeMapTabParam(optActiveShapeMapTab) +&
      InferenceParam(optInference) => {
      val tp = TriggerModeParam(
        optTriggerMode,
        optShapeMap,
        optShapeMapFormat,
        optShapeMapURL,
        optShapeMapFormat, // TODO: Maybe a more specific param for URL format?
        optShapeMapFile,
        optShapeMapFormat, // TODO: Maybe a more specific param for File format?
        optActiveShapeMapTab
      )
      val result = validate(data, optDataFormat,
        optSchema, optSchemaFormat, optSchemaEngine,
        tp, optNode, optShape, optInference)
      val default = Ok(result._1.toJson)
        // .withContentType(Some(`Content-Type`(`application/json`)))
      /*              req.headers.get(`Accept`) match {
                      case Some(ah) => {
                        logger.info(s"Accept header: $ah")
                        val hasHTML : Boolean = ah.values.exists(mr => mr.mediaRange.satisfiedBy(`text/html`))
                        if (hasHTML) {
                          val htmlStr = validationResult.toHTML
                          Ok(htmlStr).withContentType(Some(`Content-Type`(`text/html`)))
                        } else default
                      }
                      case None => default
                    } */
      default
    }

    // Contents on /swagger are directly mapped to /swagger
    case r @ GET -> _ if r.pathInfo.startsWith("/swagger/") => swagger(r).getOrElseF(NotFound())

  }


}