package es.weso.server

import es.weso.rdf.jena._
import es.weso.schema._
import org.http4s.HttpService
import org.http4s.dsl.io._
import org.http4s.server.staticcontent.ResourceService.Config
import cats.effect._
import org.http4s._
import org.http4s.twirl._
import es.weso._
import es.weso.server.QueryParams._
import ApiHelper.{query, _}
import Http4sUtils._
import es.weso.rdf._
import cats.effect.IO._
import org.http4s.multipart._
import io.circe.Json
import Defaults._

object WebService {

  // Get the static content
  private val static: HttpService[IO] = staticResource[IO](Config("/static", "/static"))
  private val views: HttpService[IO] = staticResource(Config("/staticviews", "/"))


  val webService: HttpService[IO] = HttpService[IO] {

    case req@GET -> Root => {
      Ok(html.index())
    }

    case req@GET -> Root / "dataConversions" :?
      OptDataParam(optData) +&
      DataFormatParam(optDataFormat) +&
      InferenceParam(optInference) +&
      OptEndpointParam(optEndpoint) +&
      OptActiveDataTabParam(optActiveDataTab) +&
      TargetDataFormatParam(optTargetDataFormat) => {
      Ok(html.dataConversions(
        optData,
        availableDataFormats,
        optDataFormat.getOrElse(defaultDataFormat),
        availableInferenceEngines,
        optInference.getOrElse(defaultInference),
        optEndpoint,
        optTargetDataFormat.getOrElse(defaultDataFormat),
        optActiveDataTab.getOrElse(defaultActiveDataTab),
        dataConvert(optData, optDataFormat, optTargetDataFormat)))

    }

    case req@POST -> Root / "dataConversions" => {
      req.decode[Multipart[IO]] { m =>
        val partsMap = PartsMap(m.parts)
        for {
          maybeData <- DataParam.mkData(partsMap)
          response <- maybeData match {
            case Left(msg) => BadRequest(s"Error obtaining data: $msg")
            case Right((rdf,dp)) => {
              val targetFormat = dp.targetDataFormat.getOrElse(defaultDataFormat)
              val result = rdf.serialize(targetFormat).map(Some(_))
              Ok(html.dataConversions(dp.data,
                  availableDataFormats,
                  dp.dataFormat.getOrElse(defaultDataFormat),
                  availableInferenceEngines,
                  dp.inference.getOrElse(defaultInference),
                  dp.endpoint,
                  dp.targetDataFormat.getOrElse(defaultDataFormat),
                  dp.activeDataTab.getOrElse(defaultActiveDataTab),
                  result))
            }
          }
        } yield response
      }
    }

    case req@GET -> Root / "dataInfo" :?
      OptDataParam(optData) +&
      DataFormatParam(optDataFormat) +&
      InferenceParam(optInference) +&
      OptEndpointParam(optEndpoint) +&
      OptActiveDataTabParam(optActiveDataTab) => {
      RDFAsJenaModel.fromChars(optData.getOrElse(""), optDataFormat.getOrElse(defaultDataFormat)).fold(
        str => BadRequest(str),
        rdf => Ok(html.dataInfo(dataInfo(rdf),
          optData,
          availableDataFormats,
          defaultDataFormat,
          availableInferenceEngines,
          optInference.getOrElse(defaultInference),
          optEndpoint,
          optActiveDataTab.getOrElse(defaultActiveDataTab))))
    }

    case req@POST -> Root / "dataInfo" => {
      req.decode[Multipart[IO]] { m =>
        val partsMap = PartsMap(m.parts)
        for {
          maybeData <- DataParam.mkData(partsMap)
          response <- maybeData match {
            case Left(str) => BadRequest (s"Error obtaining data: $str")
            case Right((rdf,dp)) =>
              Ok(html.dataInfo(dataInfo(rdf),
                dp.data,
                availableDataFormats,
                dp.dataFormat.getOrElse(defaultDataFormat),
                availableInferenceEngines,
                dp.inference.getOrElse(defaultInference),
                dp.endpoint,
                dp.activeDataTab.getOrElse(defaultActiveDataTab)))
          }
        } yield response
      }
    }

    case req@POST -> Root / "schemaConversions" =>
      req.decode[Multipart[IO]] { m => {
        val partsMap = PartsMap(m.parts)
        for {
          maybePair <- SchemaParam.mkSchema(partsMap, None)
          optTargetSchemaFormat <- partsMap.optPartValue("targetSchemaFormat")
          optTargetSchemaEngine <- partsMap.optPartValue("targetSchemaEngine")
          response <- maybePair match {
            case Left(msg) => BadRequest(s"Error obtaining schema: $msg")
            case Right((schema, sp)) => {
              Ok(html.schemaConversions(
                sp.schema,
                availableSchemaFormats,
                sp.schemaFormat.getOrElse(defaultSchemaFormat),
                availableSchemaEngines,
                sp.schemaEngine.getOrElse(defaultSchemaEngine),
                optTargetSchemaFormat.getOrElse(defaultSchemaFormat),
                optTargetSchemaEngine.getOrElse(defaultSchemaEngine),
                sp.activeSchemaTab.getOrElse(defaultActiveSchemaTab),
                schemaConvert(sp.schema,sp.schemaFormat,sp.schemaEngine,
                  optTargetSchemaFormat,optTargetSchemaEngine,
                  ApiHelper.getBase))
              )
            }

          }
        } yield response
      }
      }

    case req@GET -> Root / "schemaConversions" :?
      OptSchemaParam(optSchema) +&
        SchemaURLParam(optSchemaURL) +&
        SchemaFormatParam(optSchemaFormat) +&
        SchemaEngineParam(optSchemaEngine) +&
        TargetSchemaFormatParam(optTargetSchemaFormat) +&
        TargetSchemaEngineParam(optTargetSchemaEngine) +&
        OptActiveSchemaTabParam(optActiveSchemaTab) => {
/*      val baseUri = req.uri
      val eitherSchema: Either[String, Option[String]] = optSchema match {
        case None => optSchemaURL match {
          case None => Right(None)
          case Some(schemaURL) => resolveUri(baseUri, schemaURL)
        }
        case Some(schemaStr) => Right(Some(schemaStr))
      } */


      Ok(html.schemaConversions(
        optSchema,
        availableSchemaFormats,
        optSchemaFormat.getOrElse(defaultSchemaFormat),
        availableSchemaEngines,
        optSchemaEngine.getOrElse(defaultSchemaEngine),
        optTargetSchemaFormat.getOrElse(defaultSchemaFormat),
        optTargetSchemaEngine.getOrElse(defaultSchemaEngine),
        optActiveSchemaTab.getOrElse(defaultActiveSchemaTab),
        schemaConvert(optSchema,optSchemaFormat,optSchemaEngine,
          optTargetSchemaFormat,optTargetSchemaEngine,
          ApiHelper.getBase))
      )
    }

    case req@POST -> Root / "schemaInfo" =>
      req.decode[Multipart[IO]] { m => {
        val partsMap = PartsMap(m.parts)
        for {
          maybePair <- SchemaParam.mkSchema(partsMap, None)
          response <- maybePair match {
           case Left(msg) => BadRequest(s"Error obtaining schema: $msg")
           case Right((schema, sp)) => {
             val info: Json = Json.fromString(s"Schema parsed OK")
             Ok(html.schemaInfo(
               Some(info),
               sp.schema,
               availableSchemaFormats,
               sp.schemaFormat.getOrElse(defaultSchemaFormat),
               availableSchemaEngines,
               sp.schemaEngine.getOrElse(defaultSchemaEngine),
               sp.activeSchemaTab.getOrElse(defaultActiveSchemaTab)))
           }
        }
      } yield response
    }
    }

    case req@GET -> Root / "schemaInfo" :?
      OptSchemaParam(optSchema) => {
      val info = Json.fromString(s"Schema $optSchema")
      Ok(html.schemaInfo(Some(info),
        optSchema,
        availableSchemaFormats,
        defaultSchemaFormat,
        availableSchemaEngines,
        defaultSchemaEngine,
        defaultActiveSchemaTab))
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
      ExamplesParam(examples) +& ManifestURLParam(manifestURL) => {
      (examples,manifestURL) match {
        case (None,None) => BadRequest(s"Missing parameter 'examples' or 'manifestURL'")
        case (Some(ex),None) => Ok(html.load(ex))
        case (None,Some(ex)) => Ok(html.load(ex))
        case (Some(ex1),Some(ex2)) =>
          if (ex1 == ex2) Ok(html.load(ex1))
          else BadRequest(s"Parameter 'examples' and 'manifestURL' are different, select one of them")
      }
      // val xs = List(examples,manifestURL).collectFirst(x => x.)

    }

    case req@POST -> Root / "validate" =>
      req.decode[Multipart[IO]] { m => {
        println(s"POST validate")
        val partsMap = PartsMap(m.parts)
        println(s"POST validate partsMap. $partsMap")
        for {
          maybeData <- DataParam.mkData(partsMap)
          response <- maybeData match {
            case Left(msg) => BadRequest(s"Error obtaining data: $msg")
            case Right((rdf, dp)) => for {
                maybePair <- SchemaParam.mkSchema(partsMap, Some(rdf))
                response <- maybePair match {
                  case Left(msg) => BadRequest(s"Error obtaining schema: $msg")
                  case Right((schema, sp)) => for {
                    tp <- {
                      println(s">>>>>>>>>>>>>>>> Data: ${rdf.serialize()}\ndata string:${dp.data}")
                      println(s">>>>>>>>>>>>>>>> Schema: $schema")
                      TriggerModeParam.mkTriggerModeParam(partsMap)
                    }
                    schemaEmbedded = {
                      println(s">>>> Trigger: $tp")
                      getSchemaEmbedded(sp)
                    }
                    optShapeMapStr = tp.getShapeMap._1
                    (result, maybeTriggerMode) = validate(dp.data.getOrElse(""), dp.dataFormat,
                      sp.schema, sp.schemaFormat, sp.schemaEngine,
                      tp.triggerMode,
                      None, None,
                      optShapeMapStr, dp.inference)
                    r <- validateResponse(result,dp,sp,tp)
                  } yield r
                }
          } yield response
        }
      } yield response
    }
  }

    case req@GET -> Root / "validate" :?
      OptExamplesParam(optExamples) +&
      OptDataParam(optData) +&
      OptDataURLParam(optDataURL) +&
      DataFormatParam(optDataFormat) +&
      OptSchemaParam(optSchema) +&
      SchemaURLParam(optSchemaURL) +&
      SchemaFormatParam(optSchemaFormat) +&
      SchemaEngineParam(optSchemaEngine) +&
      OptTriggerModeParam(optTriggerMode) +&
      NodeParam(optNode) +&
      ShapeParam(optShape) +&
      ShapeMapParameter(optShapeMap) +&
      ShapeMapFormatParam(optShapeMapFormat) +&
      SchemaEmbedded(optSchemaEmbedded) +&
      InferenceParam(optInference) +&
      OptEndpointParam(optEndpoint) +&
      OptActiveDataTabParam(optActiveDataTab) +&
      OptActiveSchemaTabParam(optActiveSchemaTab) +&
      OptActiveShapeMapTabParam(optActiveShapeMapTab) => {

      if (optExamples.isDefined) {
        Ok(html.load(optExamples.get))
      } else {
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

        println(s"Either schema: $eitherSchema")
        println(s"OptSchema: $optSchema")
        println(s"OptSchemaFormat: $optSchemaFormat")

        val eitherResult = for {
          data <- eitherData
          schema <- eitherSchema
        } yield {
          data.map(validate(_, optDataFormat,
            schema, optSchemaFormat, optSchemaEngine,
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
              optSchemaFormat.getOrElse(defaultSchemaFormat),
              availableSchemaEngines,
              optSchemaEngine.getOrElse(Schemas.shEx.name),
              availableTriggerModes,
              triggerMode.name,
              shapeMap,
              availableShapeMapFormats,
              optShapeMapFormat.getOrElse(defaultShapeMapFormat),
              optSchemaEmbedded.getOrElse(defaultSchemaEmbedded),
              availableInferenceEngines,
              optInference.getOrElse("NONE"),
              optEndpoint,
              optActiveDataTab.getOrElse(defaultActiveDataTab),
              optActiveSchemaTab.getOrElse(defaultActiveSchemaTab),
              optActiveShapeMapTab.getOrElse(defaultActiveShapeMapTab)
            ))
          }
        }
      }
    }
    case req@GET -> Root / "query" :?
      OptDataParam(optData) +&
        DataFormatParam(optDataFormat) +&
        OptQueryParam(optQuery) +&
        InferenceParam(optInference) +&
        OptEndpointParam(optEndpoint) +&
        OptActiveDataTabParam(optActiveDataTab) +&
        OptActiveQueryTabParam(optActiveQueryTab)
        => {
      val result = query(optData.getOrElse(""), optDataFormat, optQuery, optInference)
      Ok(html.query(
        result,
        optData,
        availableDataFormats,
        optDataFormat.getOrElse(defaultDataFormat),
        optQuery,
        availableInferenceEngines,
        optInference.getOrElse("NONE"),
        optEndpoint,
        optActiveDataTab.getOrElse(defaultActiveDataTab),
        optActiveQueryTab.getOrElse(defaultActiveQueryTab)
      ))
    }

    case req@POST -> Root / "query" => {
      req.decode[Multipart[IO]] { m => {
        val partsMap = PartsMap(m.parts)
        for {
          maybeData <- DataParam.mkData(partsMap)
          response <- maybeData match {
            case Left(msg) => BadRequest(s"Error obtaining data: $msg")
            case Right((rdf, dp)) => for {
              maybePair <- SparqlQueryParam.mkQuery(partsMap)
              response <- maybePair match {
                case Left(msg) => BadRequest(s"Error obtaining query: $msg")
                case Right((queryStr, qp)) => {
                  val optQueryStr = qp.query.map(_.str)
                  val result = rdf.queryAsJson(optQueryStr.getOrElse(""))
                  println(s"Result: ${result}")
                  Ok(html.query(
                    result,
                    dp.data,
                    availableDataFormats,
                    dp.dataFormat.getOrElse(defaultDataFormat),
                    optQueryStr,
                    availableInferenceEngines,
                    dp.inference.getOrElse(defaultInference),
                    dp.endpoint,
                    dp.activeDataTab.getOrElse(defaultActiveDataTab),
                    qp.activeQueryTab.getOrElse(defaultActiveQueryTab)
                  ))
                }
              }
            } yield response
          }
        } yield response
       }
      }
    }

    // Contents on /static are mapped to /static
    case r@GET -> _ if r.pathInfo.startsWith("/static") => static(r).getOrElseF(NotFound())

    // case r @ GET -> _ if r.pathInfo.startsWith("/swagger.json") => views(r)

    // When accessing to a folder (ends by /) append index.scala.html
    case r@GET -> _ if r.pathInfo.endsWith("/") =>
      webService(r.withPathInfo(r.pathInfo + "index.scala.html")).getOrElseF(NotFound())

    case r@GET -> _ =>
      val rr = if (r.pathInfo.contains('.')) r else r.withPathInfo(r.pathInfo + ".html")
      views(rr).getOrElseF(NotFound())
  }

  def err[A](str: String): Either[String, A] = {
    Left(str)
  }

  private def validateResponse(result: Result,
                               dp: DataParam,
                               sp: SchemaParam,
                               tp: TriggerModeParam): IO[Response[IO]] =
    Ok(html.validate(Some(result),dp.data,
    availableDataFormats,
    dp.dataFormat.getOrElse(defaultDataFormat),
    sp.schema,
    availableSchemaFormats,
    sp.schemaFormat.getOrElse(defaultSchemaFormat),
    availableSchemaEngines,
    sp.schemaEngine.getOrElse(defaultSchemaEngine),
    availableTriggerModes,
    tp.triggerMode.getOrElse(defaultTriggerMode),
    tp.shapeMap,
    availableShapeMapFormats,
    tp.shapeMapFormat.getOrElse(defaultShapeMapFormat),
    getSchemaEmbedded(sp),
    availableInferenceEngines,
    dp.inference.getOrElse(defaultInference),
    dp.endpoint,
    dp.activeDataTab.getOrElse(defaultActiveDataTab),
    sp.activeSchemaTab.getOrElse(defaultActiveSchemaTab),
    tp.activeShapeMapTab.getOrElse(defaultActiveShapeMapTab)
  ))

  private def getSchemaEmbedded(sp: SchemaParam): Boolean = {
    sp.schemaEmbedded match {
      case Some(true) => true
      case Some(false) => false
      case None => defaultSchemaEmbedded
    }
  }

  private def dataInfo(rdf: RDFReasoner): Option[Json] = {
    Some(Json.fromFields(
      List(
        ("statements", Json.fromString(rdf.getNumberOfStatements().fold(identity,_.toString))),
        ("nodesPrefixMap", ApiHelper.prefixMap2Json(rdf.getPrefixMap()))
      )
    ))
  }
}