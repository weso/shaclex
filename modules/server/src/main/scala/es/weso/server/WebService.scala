package es.weso.server

import es.weso.rdf.jena.{Endpoint, RDFAsJenaModel}
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
import cats.data.EitherT
import es.weso.rdf._
import cats.effect.IO._
import es.weso.server.WebService.{DataParam, PartsMap, parts2Map}
import fs2.text.utf8Decode
import org.http4s.multipart.{Multipart, Part}
import cats.implicits._
import es.weso.utils.FileUtils

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
  val defaultInference = "None"
  val defaultActiveDataTab = "#dataTextArea"
  val defaultActiveSchemaTab = "#schemaTextArea"

  val webService: HttpService[IO] = HttpService[IO] {

    case req@GET -> Root => {
      Ok(html.index())
    }

    case req@GET -> Root / "dataConversions" :?
      OptDataParam(optData) +&
        DataFormatParam(optDataFormat) +&
        InferenceParam(optInference) +&
        OptActiveDataTabParam(optActiveDataTab) +&
        TargetDataFormatParam(optTargetDataFormat) => {
      val targetDataFormat = optTargetDataFormat.getOrElse(defaultDataFormat)
      Ok(html.dataConversions(
        optData,
        availableDataFormats,
        optDataFormat.getOrElse(defaultDataFormat),
        availableInferenceEngines,
        optInference.getOrElse(defaultInference),
        optTargetDataFormat.getOrElse(defaultDataFormat),
        optActiveDataTab.getOrElse(defaultActiveDataTab),
        dataConvert(optData, optDataFormat, optTargetDataFormat)))

    }

    case req@POST -> Root / "dataConversions" => {
      req.decode[Multipart[IO]] { m =>
        val partsMap = parts2Map(m.parts)
        for {
          maybeData <- mkData(partsMap)
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
        OptActiveDataTabParam(optActiveDataTab) => {
      RDFAsJenaModel.fromChars(optData.getOrElse(""), optDataFormat.getOrElse(defaultDataFormat)).fold(
        str => BadRequest(str),
        rdf => Ok(html.dataInfo(
          optData,
          availableDataFormats,
          defaultDataFormat,
          availableInferenceEngines,
          optInference.getOrElse(defaultInference),
          optActiveDataTab.getOrElse(defaultActiveDataTab))))
    }

    case req@POST -> Root / "dataInfo" => {
      req.decode[Multipart[IO]] { m =>
        val partsMap = parts2Map(m.parts)
        for {
          maybeData <- mkData(partsMap)
          response <- maybeData match {
            case Left(str) => BadRequest (s"Error obtaining data: $str")
            case Right((rdf,dp)) =>
              Ok(html.dataInfo(
                dp.data,
                availableDataFormats,
                dp.dataFormat.getOrElse(defaultDataFormat),
                availableInferenceEngines,
                dp.inference.getOrElse(defaultInference),
                dp.activeDataTab.getOrElse(defaultActiveDataTab)))
          }
        } yield response
      }
    }

    case req@GET -> Root / "schemaConversions" :?
      OptSchemaParam(optSchema) +&
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

    case req@POST -> Root / "validate" =>
      req.decode[Multipart[IO]] { m => {
        val partsMap = parts2Map(m.parts)
        for {
          maybeData <- mkData(partsMap)
          response <- maybeData match {
            case Left(msg) => BadRequest(s"Error obtaining data: $msg")
            case Right((rdf, dp)) => for {
                maybePair <- mkSchema(partsMap, Some(rdf))
                response <- maybePair match {
                  case Left(msg) => BadRequest(s"Error obtaining schema: $msg")
                  case Right((schema, sp)) => for {
                    tp <- mkTriggerModeParam(partsMap)
                    schemaEmbedded = getSchemaEmbedded(sp)
                    (result, maybeTriggerMode) = validate(dp.data.getOrElse(""), dp.dataFormat,
                      sp.schema, sp.schemaFormat, sp.schemaEngine,
                      tp.triggerMode,
                      None, None,
                      tp.shapeMap, dp.inference)
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
        ShapeMapParam(optShapeMap) +&
        SchemaEmbedded(optSchemaEmbedded) +&
        InferenceParam(optInference) +&
        OptActiveDataTabParam(optActiveDataTab) => {

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
              optInference.getOrElse("NONE"),
              optActiveDataTab.getOrElse(defaultActiveDataTab),
              defaultActiveSchemaTab
            ))
          }
        }
      }
    }
    case req@GET -> Root / "query" :?
      OptDataParam(optData) +&
        DataFormatParam(optDataFormat) +&
        OptQueryParam(optQuery) +&
        InferenceParam(optInference) => {
      val result = query(optData.getOrElse(""), optDataFormat, optQuery, optInference)
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

        val result = query(optData.getOrElse(""), optDataFormat, optQuery, optInference)

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


  private def extendWithInference(rdf: RDFReasoner,
                                  optInference: Option[String]
                                 ): Either[String,RDFReasoner] = {
    rdf.applyInference(optInference.getOrElse("None")).fold(
      msg => Left(s"Error applying inference to RDF: $msg"),
      (newRdf: RDFReasoner) => Right(newRdf)
    )
  }

  case class SchemaParam(schema: Option[String],
                         schemaURL: Option[String],
                         schemaFile: Option[String],
                         schemaFormat: Option[String],
                         schemaEngine: Option[String],
                         schemaEmbedded: Option[Boolean],
                         targetSchemaFormat: Option[String],
                         activeSchemaTab: Option[String]
                        )

  private def mkSchema(partsMap: PartsMap, data: Option[RDFReasoner]
                      ): IO[Either[String,(Schema, SchemaParam)]] = for {
    sp <- mkSchemaParam(partsMap)
  } yield {
    val (maybeStr, maybeSchema) = getSchema(sp, data)
    maybeSchema match {
      case Left(str) => Left(str)
      case Right(schema) => Right((schema, sp.copy(schema = maybeStr)))
    }
  }

  private def mkSchemaParam(partsMap: PartsMap): IO[SchemaParam] = for {
    schema <- optPartValue("schema", partsMap)
    schemaURL <- optPartValue("schemaURL", partsMap)
    schemaFile <- optPartValue("schemaFile", partsMap)
    schemaFormat <- optPartValue("schemaFormat", partsMap)
    schemaEngine <- optPartValue("schemaEngine", partsMap)
    targetSchemaFormat <- optPartValue("targetSchemaFormat", partsMap)
    activeSchemaTab <- optPartValue("activeSchemaTab", partsMap)
    schemaEmbedded <- optPartValueBoolean("schemaEmbedded", partsMap)
  } yield SchemaParam(schema, schemaURL, schemaFile, schemaFormat, schemaEngine, schemaEmbedded, targetSchemaFormat, activeSchemaTab)

  private def getSchema(sp: SchemaParam, data: Option[RDFReasoner]): (Option[String],Either[String,Schema]) = {
    val base = Some(FileUtils.currentFolderURL)
    sp.schemaEmbedded match {
       case Some(true) => data match {
          case None => (None, Left(s"Schema embedded but no data found"))
          case Some(rdf) => Schemas.fromRDF(rdf, sp.schemaEngine.getOrElse(defaultSchemaEngine)) match {
            case Left(str) => (None,Left(s"Error obtaining schema from RDF $rdf"))
            case Right(schema) => schema.serialize(sp.schemaFormat.getOrElse(defaultSchemaFormat)) match {
              case Left(str) => (None, Right(schema))
              case Right(str) => (Some(str), Right(schema))
            }
         }
       }
       case _ => {
          println(s"######## Schema not embedded...Active schema tab: ${sp.activeSchemaTab}")
          sp.activeSchemaTab.getOrElse(defaultActiveSchemaTab) match {
            case "#schemaUrl" => {
              println(s"######## SchemaUrl: ${sp.schemaURL}")
              sp.schemaURL match {
                case None => (None, Left(s"Non value for dataURL"))
                case Some(schemaUrl) => {
                  val schemaFormat = sp.schemaFormat.getOrElse(defaultSchemaFormat)
                  (None,Left(s"Not implemented obtaining $schemaUrl with $schemaFormat"))
                }
              }
            }
            case "#schemaFile" => {
              sp.schemaFile match {
                case None => (None, Left(s"No value for schemaFile"))
                case Some(schemaStr) =>
                  val schemaFormat = sp.schemaFormat.getOrElse(defaultSchemaFormat)
                  val schemaEngine = sp.schemaEngine.getOrElse(defaultSchemaEngine)
                  Schemas.fromString(schemaStr, schemaFormat, schemaEngine, base) match {
                    case Left(msg) => (Some(schemaStr), Left(s"Error parsing file: $msg"))
                    case Right(schema) => (Some(schemaStr), Right(schema))
                  }
              }
            }
            case "#schemaTextArea" => {
              sp.schema match {
                case None => (None, Left(s"No value for schema in textArea"))
                case Some(schemaStr) => Schemas.fromString(schemaStr,sp.schemaFormat.getOrElse(defaultSchemaFormat),
                    sp.schemaEngine.getOrElse(defaultSchemaEngine),base) match {
                    case Left(msg) => (Some(schemaStr),Left(msg))
                    case Right(schema) => (Some(schemaStr),Right(schema))
                  }
              }
            }
            case other => (None,Left(s"Not implemented getSchema: $other"))
          }
        }
      }
  }


  case class DataParam(data: Option[String],
                       dataURL: Option[String],
                       dataFile: Option[String],
                       endpoint: Option[String],
                       dataFormat: Option[String],
                       inference: Option[String],
                       targetDataFormat: Option[String],
                       activeDataTab: Option[String]
                      )

  private def mkData(partsMap: PartsMap): IO[Either[String, (RDFReasoner, DataParam)]] = for {
    dp <- mkDataParam(partsMap)
  } yield {
    val (maybeStr, maybeData) = getData(dp)
    maybeData match {
      case Left(str) => Left(str)
      case Right(data) => Right((data, dp.copy(data = maybeStr)))
    }
  }

  private def getData(dp: DataParam): (Option[String], Either[String,RDFReasoner]) =
     dp.activeDataTab.getOrElse(defaultActiveDataTab) match {
        case d @"#dataUrl" => {
          dp.dataURL match {
            case None => (None, Left(s"Non value for dataURL"))
            case Some(dataUrl) => {
              val dataFormat = dp.dataFormat.getOrElse(defaultDataFormat)
              RDFAsJenaModel.fromURI(dataUrl,dataFormat) match {
                case Left(str) => (None,Left(s"Error obtaining $dataUrl with $dataFormat: $str"))
                case Right(rdf) => {
                  (rdf.serialize(dataFormat).toOption, extendWithInference(rdf,dp.inference))
                }
              }
            }
          }
        }
        case "#dataFile" => {
          dp.dataFile match {
            case None => (None, Left(s"No value for dataFile"))
            case Some(dataStr) =>
              val dataFormat = dp.dataFormat.getOrElse(defaultDataFormat)
              RDFAsJenaModel.fromChars(dataStr, dataFormat, None) match {
                case Left(msg) => (Some(dataStr), Left(msg))
                case Right(rdf) => {
                  (Some(dataStr), extendWithInference(rdf, dp.inference))
                }
              }
          }
        }
        case d @ "#dataEndpoint" => {
          dp.endpoint match {
            case None => (None, Left(s"No value for endpoint"))
            case Some(endpointUrl) => {
              Endpoint.fromString(endpointUrl) match {
                case Left(str) => (None, Left(s"Error creating endpoint: $endpointUrl"))
                case Right(endpoint) => {
                  (None, extendWithInference(endpoint,dp.inference))
                }
              }
            }
          }
        }
        case d @ "#dataTextArea" => {
          dp.data match {
            case None => (None, Right(RDFAsJenaModel.empty))
            case Some(data) => {
              RDFAsJenaModel.fromChars(data, dp.dataFormat.getOrElse(defaultDataFormat), None) match {
                case Left(msg) => (Some(data), Left(msg))
                case Right(rdf) => {
                  (Some(data), extendWithInference(rdf,dp.inference))
                }
              }
            }
          }
        }
        case other => (None, Left(s"Unknown value for activeDataTab: $other"))
  }

  private def mkDataParam(partsMap: PartsMap): IO[DataParam] = for {
    data <- optPartValue("data", partsMap)
    dataURL <- optPartValue("dataURL", partsMap)
    dataFile <- optPartValue("dataFile", partsMap)
    endpoint <- optPartValue("endpoint", partsMap)
    dataFormat <- optPartValue("dataFormat", partsMap)
    inference <- optPartValue("inference", partsMap)
    targetDataFormat <- optPartValue("targetDataFormat", partsMap)
    activeDataTab <- optPartValue("rdfDataActiveTab", partsMap)
  } yield DataParam(data,dataURL,dataFile,endpoint,dataFormat,inference,targetDataFormat,activeDataTab)

  type PartsMap = Map[String,Part[IO]]

  private def parts2Map(ps: Vector[Part[IO]]): PartsMap = {
    ps.filter(_.name.isDefined).map(p => (p.name.get,p)).toMap
  }

  private def optPartValue(key: String, partsMap: PartsMap): IO[Option[String]] = partsMap.get(key) match {
    case Some(part) => part.body.through(utf8Decode).runFoldMonoid.map(Some(_))
    case None => IO(None)
  }

  private def optPartValueBoolean(key: String, partsMap: PartsMap): IO[Option[Boolean]] = partsMap.get(key) match {
    case Some(part) => part.body.through(utf8Decode).runFoldMonoid.map(_ match {
      case "true" => Some(true)
      case "false" => Some(false)
      case _ => None
    })
    case None => IO(None)
  }

  private def mkTriggerModeParam(partsMap: PartsMap): IO[TriggerModeParam] = for {
    optTriggerMode <- optPartValue("triggerMode", partsMap)
    optShapeMap <- optPartValue("shapeMap", partsMap)
  } yield TriggerModeParam(optTriggerMode, optShapeMap)

  case class TriggerModeParam(triggerMode: Option[String],
                              shapeMap: Option[String])

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
    getSchemaEmbedded(sp),
    availableInferenceEngines,
    dp.inference.getOrElse(defaultInference),
    dp.activeDataTab.getOrElse(defaultActiveDataTab),
    sp.activeSchemaTab.getOrElse(defaultActiveSchemaTab)
  ))

  private def getSchemaEmbedded(sp: SchemaParam): Boolean = {
    sp.schemaEmbedded match {
      case Some(true) => true
      case Some(false) => false
      case None => defaultSchemaEmbedded
    }
  }
}