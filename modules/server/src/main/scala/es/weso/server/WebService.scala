package es.weso.server

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema._
import org.http4s.HttpService
import org.http4s.dsl.io._
import org.http4s.server.staticcontent.ResourceService.Config

import cats.effect._
import org.http4s._
import org.http4s.twirl._
import es.weso._
import es.weso.server.QueryParams._
import org.log4s.getLogger
import ApiHelper._
import Http4sUtils._

object WebService {

  private val logger = getLogger

  // Get the static content
  private val static: HttpService[IO] = staticResource[IO](Config("/static", "/static"))
  private val views: HttpService[IO] = staticResource(Config("/staticviews", "/"))


  val availableDataFormats = DataFormats.formatNames.toList
  val defaultDataFormat = DataFormats.defaultFormatName
  val availableSchemaFormats = Schemas.availableFormats
  val defaultSchemaFormat = Schemas.defaultSchemaFormat
  val availableSchemaEngines = Schemas.availableSchemaNames
  val defaultSchemaEngine = Schemas.defaultSchemaName
  val availableTriggerModes = Schemas.availableTriggerModes
  val defaultTriggerMode = Schemas.defaultTriggerMode
  val availableInferenceEngines = RDFAsJenaModel.empty.availableInferenceEngines
  val defaultSchemaEmbedded = false

  val webService: HttpService[IO] = HttpService[IO] {

    case req@GET -> Root => {
      Ok(html.index())
    }

    case req@GET -> Root / "dataConversions" :?
      OptDataParam(optData) +&
        DataFormatParam(optDataFormat) +&
        TargetDataFormatParam(optTargetDataFormat) => {
      Ok(html.dataConversions(
        optData,
        availableDataFormats,
        optDataFormat.getOrElse(defaultDataFormat),
        optTargetDataFormat.getOrElse(defaultDataFormat),
        dataConvert(optData, optDataFormat, optTargetDataFormat)))
    }

    case req@GET -> Root / "dataInfo" => {
      Ok(html.dataInfo(
        availableDataFormats,
        defaultDataFormat))
    }

    case req@GET -> Root / "schemaConversions" :?
      SchemaParam(optSchema) +&
        SchemaFormatParam(optSchemaFormat) +&
        SchemaEngineParam(optSchemaEngine) +&
        TargetSchemaFormatParam(optTargetSchemaFormat) +&
        TargetSchemaEngineParam(optTargetSchemaEngine) => {
      Ok(html.schemaConversions(
        None,
        optSchema,
        availableSchemaFormats,
        optSchemaFormat.getOrElse(defaultSchemaFormat),
        availableSchemaEngines,
        optSchemaEngine.getOrElse(defaultSchemaEngine),
        optTargetSchemaFormat.getOrElse(defaultSchemaFormat),
        optTargetSchemaEngine.getOrElse(defaultSchemaEngine)))
    }

    case req@GET -> Root / "schemaInfo" => {
      Ok(html.schemaInfo(
        availableSchemaFormats,
        defaultSchemaFormat,
        availableSchemaEngines,
        defaultSchemaEngine))
    }

    case req@GET -> Root / "dataOptions" => {
      Ok(html.dataOptions(
        availableDataFormats,
        defaultDataFormat))
    }

    case req@GET -> Root / "schemaOptions" => {
      Ok(html.schemaOptions(
        availableSchemaFormats,
        defaultSchemaFormat,
        availableSchemaEngines,
        defaultSchemaEngine,
        availableTriggerModes,
        defaultTriggerMode))
    }

    case req@GET -> Root / "about" => {
      Ok(html.about())
    }

    case req@GET -> Root / "load" :?
      ExamplesParam(examples) => {
      println(s"Examples: $examples")
      Ok(html.load(examples))
    }

    case req@POST -> Root / "validate" => {
      req.decode[UrlForm] { m => {
        val values = m.values
        logger.info(s"POST data: ${m.values.mkString("\n")}")
        val data = values.get("data").map(_.head).getOrElse("")
        val optSchema = values.get("schema").map(_.head)
        val optDataFormat = values.get("dataFormat").map(_.head)
        val optSchemaFormat = values.get("schemaFormat").map(_.head)
        val optSchemaEngine = values.get("schemaEngine").map(_.head)
        val optTriggerMode = values.get("triggerMode").map(_.head)
        val optShapeMap = values.get("shapeMap").map(_.head)
        val optInference = values.get("inference").map(_.head)
        val optSchemaEmbedded = values.get("schemaEmbedded").map(_.head)

        val schemaEmbedded = optSchemaEmbedded match {
          case Some("true") => true
          case Some("false") => false
          case Some(msg) => {
            logger.info(s"Unsupported value for schema embedded: $msg")
            defaultSchemaEmbedded
          }
          case None => defaultSchemaEmbedded
        }

        val (result,maybeTriggerMode) = validate(data, optDataFormat,
          optSchema, optSchemaFormat, optSchemaEngine,
          optTriggerMode,
          None, None, optShapeMap,
          optInference)
        val triggerMode = maybeTriggerMode.getOrElse(ValidationTrigger.default)
        val shapeMap = triggerMode match {
          case TargetDeclarations => None
          case ShapeMapTrigger(sm) => Some(sm.toString)
        }
        Ok(html.validate(
          Some(result),
          Some(data),
          availableDataFormats,
          optDataFormat.getOrElse(defaultDataFormat),
          optSchema,
          availableSchemaFormats,
          optSchemaFormat.getOrElse(Schemas.shEx.defaultFormat),
          availableSchemaEngines,
          optSchemaEngine.getOrElse(Schemas.shEx.name),
          availableTriggerModes,
          triggerMode.name,
          shapeMap,
          schemaEmbedded,
          availableInferenceEngines,
          optInference.getOrElse("NONE")
        ))
       }
      }
    }


    case req@GET -> Root / "validate" :?
      OptDataParam(optData) +&
      OptDataURLParam(optDataURL) +&
      DataFormatParam(optDataFormat) +&
      SchemaParam(optSchema) +&
      SchemaURLParam(optSchemaURL) +&
      SchemaFormatParam(optSchemaFormat) +&
      SchemaEngineParam(optSchemaEngine) +&
      TriggerModeParam(optTriggerMode) +&
      NodeParam(optNode) +&
      ShapeParam(optShape) +&
      ShapeMapParam(optShapeMap) +&
      SchemaEmbedded(optSchemaEmbedded) +&
      InferenceParam(optInference) => {

      val baseUri = req.uri

      println(s"BaseURI: $baseUri")

      val eitherData: Either[String, Option[String]] = optData match {
        case None => optDataURL match {
          case None => Right(None)
          case Some(dataURL) => resolveUri(baseUri, dataURL)
        }
        case Some(dataStr) => Right(Some(dataStr))
      }

      val eitherSchema: Either[String, Option[String]] = optSchema match {
        case None => optSchemaURL match {
          case None => Right(None)
          case Some(schemaURL) => resolveUri(baseUri, schemaURL)
        }
        case Some(schemaStr) => Right(Some(schemaStr))
      }

      val eitherResult = for {
        data <- eitherData
        schema <- eitherSchema
      } yield {
        data.map(validate(_, optDataFormat,
          optSchema, optSchemaFormat, optSchemaEngine,
          optTriggerMode,
          optNode, optShape, optShapeMap,
          optInference))
      }
      eitherResult match {
        case Left(msg) => BadRequest(msg)
        case Right(result) => {
          val triggerMode: ValidationTrigger = result.
            map(_._2.getOrElse(ValidationTrigger.default)).
            getOrElse(ValidationTrigger.default)

          val shapeMap = triggerMode match {
            case TargetDeclarations => None
            case ShapeMapTrigger(sm) => Some(sm.toString)
          }
          Ok(html.validate(
            result.map(_._1),
            optData,
            availableDataFormats,
            optDataFormat.getOrElse(defaultDataFormat),
            optSchema,
            availableSchemaFormats,
            optSchemaFormat.getOrElse(Schemas.shEx.defaultFormat),
            availableSchemaEngines,
            optSchemaEngine.getOrElse(Schemas.shEx.name),
            availableTriggerModes,
            triggerMode.name,
            shapeMap,
            optSchemaEmbedded.getOrElse(defaultSchemaEmbedded),
            availableInferenceEngines,
            optInference.getOrElse("NONE")
          ))
        }
      }
    }

    case req@GET -> Root / "query" :?
      OptDataParam(optData) +&
      DataFormatParam(optDataFormat) +&
      OptQueryParam(optQuery) +&
      InferenceParam(optInference) => {
      val result = query(optData.getOrElse(""), optDataFormat,optQuery, optInference)
      Ok(html.query(
        result,
        optData,
        availableDataFormats,
        optDataFormat.getOrElse(defaultDataFormat),
        optQuery,
        availableInferenceEngines,
        optInference.getOrElse("NONE")
      ))
    }

    case req@POST -> Root / "query" => {
      req.decode[UrlForm] { m => {
        val values = m.values
        logger.info(s"POST data in query: ${m.values.mkString("\n")}")
        val optData = values.get("data").map(_.head)
        val optQuery = values.get("query").map(_.head)
        val optDataFormat = values.get("dataFormat").map(_.head)
        val optInference = values.get("inference").map(_.head)

        val result = query(optData.getOrElse(""), optDataFormat,optQuery, optInference)

        Ok(html.query(
          result,
          optData,
          availableDataFormats,
          optDataFormat.getOrElse(defaultDataFormat),
          optQuery,
          availableInferenceEngines,
          optInference.getOrElse("NONE")
        ))
      }
     }
    }

    // Contents on /static are mapped to /static
    case r @ GET -> _ if r.pathInfo.startsWith("/static") => static(r).getOrElseF(NotFound())

    // case r @ GET -> _ if r.pathInfo.startsWith("/swagger.json") => views(r)

    // When accessing to a folder (ends by /) append index.scala.html
    case r @ GET -> _ if r.pathInfo.endsWith("/") =>
      webService(r.withPathInfo(r.pathInfo + "index.scala.html")).getOrElseF(NotFound())

    case r @ GET -> _ =>
      val rr = if (r.pathInfo.contains('.')) r else r.withPathInfo(r.pathInfo + ".html")
      views(rr).getOrElseF(NotFound())
  }


}