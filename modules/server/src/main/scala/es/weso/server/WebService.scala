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
import ApiHelper.{query, _}
import Http4sUtils._
import es.weso.rdf._
import cats.effect.IO._
import fs2.text.utf8Decode
import org.http4s.multipart.{Multipart, Part}
import cats.implicits._
import es.weso.utils.FileUtils
import io.circe.Json

import scala.io.Source
import scala.util.Try

object WebService {

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
  val defaultActiveQueryTab = "#queryTextArea"

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
        val partsMap = parts2Map(m.parts)
        for {
          maybeData <- mkData(partsMap)
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
        OptEndpointParam(optEndpoint) +&
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

        println(s"Either schema: $eitherSchema")
        println(s"OptSchema: $optSchema")
        println(s"OptSchema: $optSchemaFormat")

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
              optSchemaFormat.getOrElse(Schemas.shEx.defaultFormat),
              availableSchemaEngines,
              optSchemaEngine.getOrElse(Schemas.shEx.name),
              availableTriggerModes,
              triggerMode.name,
              shapeMap,
              optSchemaEmbedded.getOrElse(defaultSchemaEmbedded),
              availableInferenceEngines,
              optInference.getOrElse("NONE"),
              optEndpoint,
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
        val partsMap = parts2Map(m.parts)
        for {
          maybeData <- mkData(partsMap)
          response <- maybeData match {
            case Left(msg) => BadRequest(s"Error obtaining data: $msg")
            case Right((rdf, dp)) => for {
              maybePair <- mkQuery(partsMap)
              response <- maybePair match {
                case Left(msg) => BadRequest(s"Error obtaining query: $msg")
                case Right((queryStr, qp)) => {
                  val result = rdf.queryAsJson(qp.query.getOrElse(""))
                  Ok(html.query(
                    result,
                    dp.data,
                    availableDataFormats,
                    dp.dataFormat.getOrElse(defaultDataFormat),
                    qp.query,
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


  private def extendWithInference(rdf: RDFReasoner,
                                  optInference: Option[String]
                                 ): Either[String,RDFReasoner] = {
    println(s"############# Applying inference $optInference")
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
    sp <- {
      println(s"PartsMap: $partsMap")
      val sp = mkSchemaParam(partsMap)
      println(s"SchemaParam: $sp")
      sp
    }
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
    println(s"SchemaEmbedded: ${sp.schemaEmbedded}")
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
                case Some(schemaUrl) => Try {
                  val uri = new java.net.URI(schemaUrl)
                  Source.fromURI(uri).mkString
                }.toEither match {
                  case Left(err) => (None,Left(s"Error obtaining schema from url $schemaUrl: ${err.getMessage} "))
                  case Right(schemaStr) => Schemas.fromString(schemaStr,
                       sp.schemaFormat.getOrElse(defaultSchemaFormat),
                       sp.schemaEngine.getOrElse(defaultSchemaEngine),
                       base) match {
                    case Left(msg) => (Some(schemaStr), Left(s"Error parsing file: $msg"))
                    case Right(schema) => (Some(schemaStr), Right(schema))
                  }

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
                       dataFormatTextarea: Option[String],
                       dataFormatUrl: Option[String],
                       dataFormatFile: Option[String],
                       inference: Option[String],
                       targetDataFormat: Option[String],
                       activeDataTab: Option[String]
                      ) {
    sealed abstract class DataInputType {
      val id: String
    }
    case object dataUrlType extends DataInputType {
      override val id = "#dataUrl"
    }
    case object dataFileType extends DataInputType {
      override val id = "#dataFile"
    }
    case object dataEndpointType extends DataInputType {
      override val id = "#dataEndpoint"
    }
    case object dataTextAreaType extends DataInputType {
      override val id = "#dataTextArea"
    }

    def parseDataTab(tab: String): Either[String,DataInputType] = {
      val inputTypes = List(dataUrlType,dataFileType,dataEndpointType,dataTextAreaType)
      inputTypes.find(_.id == tab) match {
        case Some(x) => Right(x)
        case None => Left(s"Wrong value of tab: $tab, must be one of [${inputTypes.map(_.id).mkString(",")}]")
      }
    }

    val dataFormat: Option[String] = parseDataTab(activeDataTab.getOrElse(defaultActiveDataTab)) match {
      case Right(`dataUrlType`) => dataFormatUrl
      case Right(`dataFileType`) => dataFormatFile
      case Right(`dataTextAreaType`) => dataFormatTextarea
      case _ => None
    }

  }

  private def mkData(partsMap: PartsMap): IO[Either[String, (RDFReasoner, DataParam)]] = for {
    dp <- mkDataParam(partsMap)
  } yield {
    val (maybeStr, maybeData) = getData(dp)
    maybeData match {
      case Left(str) => Left(str)
      case Right(data) => Right((data, dp.copy(data = maybeStr)))
    }
  }

  private def getData(dp: DataParam): (Option[String], Either[String,RDFReasoner]) = {
    val inputType = dp.parseDataTab(dp.activeDataTab.getOrElse(defaultActiveDataTab))
    println(s"Input type: $inputType")
    inputType match {
      case Right(dp.`dataUrlType`) => {
        dp.dataURL match {
          case None => (None, Left(s"Non value for dataURL"))
          case Some(dataUrl) => {
            val dataFormat = dp.dataFormatUrl.getOrElse(defaultDataFormat)
            RDFAsJenaModel.fromURI(dataUrl, dataFormat) match {
              case Left(str) => (None, Left(s"Error obtaining $dataUrl with $dataFormat: $str"))
              case Right(rdf) => applyInference(rdf, dp.inference, dataFormat)
            }
          }
        }
      }
      case Right(dp.`dataFileType`) => {
        dp.dataFile match {
          case None => (None, Left(s"No value for dataFile"))
          case Some(dataStr) =>
            val dataFormat = dp.dataFormatFile.getOrElse(defaultDataFormat)
            RDFAsJenaModel.fromChars(dataStr, dataFormat, None) match {
              case Left(msg) => (Some(dataStr), Left(msg))
              case Right(rdf) => {
                extendWithInference(rdf, dp.inference) match {
                  case Left(msg) => (rdf.serialize(dataFormat).toOption, Left(s"Error applying inference: $msg"))
                  case Right(newRdf) => (newRdf.serialize(dataFormat).toOption, Right(newRdf))
                }
              }
            }
        }
      }
      case Right(dp.`dataEndpointType`) => {
        dp.endpoint match {
          case None => (None, Left(s"No value for endpoint"))
          case Some(endpointUrl) => {
            Endpoint.fromString(endpointUrl) match {
              case Left(str) => (None, Left(s"Error creating endpoint: $endpointUrl"))
              case Right(endpoint) => {
                (None, extendWithInference(endpoint, dp.inference))
              }
            }
          }
        }
      }
      case Right(dp.`dataTextAreaType`) => {
        dp.data match {
          case None => (None, Right(RDFAsJenaModel.empty))
          case Some(data) => {
            val dataFormat = dp.dataFormatTextarea.getOrElse(defaultDataFormat)
            RDFAsJenaModel.fromChars(data, dataFormat, None) match {
              case Left(msg) => (Some(data), Left(msg))
              case Right(rdf) => {
                extendWithInference(rdf, dp.inference) match {
                  case Left(msg) => (rdf.serialize(dataFormat).toOption, Left(s"Error applying inference: $msg"))
                  case Right(newRdf) => (newRdf.serialize(dataFormat).toOption, Right(newRdf))
                }
              }
            }
          }
        }
      }
      case Right(other) => (None, Left(s"Unknown value for activeDataTab: $other"))
      case Left(msg) => (None, Left(msg))
    }
  }

  private def applyInference(rdf: RDFReasoner, inference: Option[String], dataFormat: String): (Option[String],Either[String,RDFReasoner]) = {
    extendWithInference(rdf, inference) match {
      case Left(msg) => (rdf.serialize(dataFormat).toOption, Left(s"Error applying inference: $msg"))
      case Right(newRdf) => (newRdf.serialize(dataFormat).toOption, Right(newRdf))
    }
  }

  private def mkDataParam(partsMap: PartsMap): IO[DataParam] = for {
    data <- optPartValue("data", partsMap)
    dataURL <- optPartValue("dataURL", partsMap)
    dataFile <- optPartValue("dataFile", partsMap)
    endpoint <- optPartValue("endpoint", partsMap)
    dataFormatTextArea <- optPartValue("dataFormatTextArea", partsMap)
    dataFormatUrl <- optPartValue("dataFormatUrl", partsMap)
    dataFormatFile <- optPartValue("dataFormatFile", partsMap)
    inference <- optPartValue("inference", partsMap)
    targetDataFormat <- optPartValue("targetDataFormat", partsMap)
    activeDataTab <- optPartValue("rdfDataActiveTab", partsMap)
  } yield {
    println(s"<<<***Data: $data")
    println(s"<<<***Data Format TextArea: $dataFormatTextArea")
    println(s"<<<***Data Format Url: $dataFormatUrl")
    println(s"<<<***Data Format File: $dataFormatFile")
    println(s"<<<***Data URL: $dataURL")
    println(s"<<<***Endpoint: $endpoint")

    val endpointRegex = "Endpoint: (.+)".r
    val finalEndpoint = endpoint.fold(data match {
      case None => None
      case Some(str) => str match {
        case endpointRegex(ep) => Some(ep)
        case _ => None
      }
    })(Some(_))
    val finalActiveDataTab = activeDataTab /* finalEndpoint match {
      case Some(endpoint) =>
        if (endpoint.length > 0) Some("#dataEndpoint")
        else activeDataTab
      case None => activeDataTab
    } */
    println(s"<<<***Endpoint: $finalEndpoint")

    DataParam(data,dataURL,dataFile,finalEndpoint,
      dataFormatTextArea,dataFormatUrl,dataFormatFile,
      inference,targetDataFormat,finalActiveDataTab
    )
  }

  type PartsMap = Map[String,Part[IO]]

  private def parts2Map(ps: Vector[Part[IO]]): PartsMap = {
    println(s"Parts: $ps")
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

  case class QueryParam(query: Option[String],
                        queryURL: Option[String],
                        queryFile: Option[String],
                        activeQueryTab: Option[String]
                       )

  type Query = String
  private def mkQuery(partsMap: PartsMap
                     ): IO[Either[String,(Query, QueryParam)]] = for {
    qp <- mkQueryParam(partsMap)
  } yield {
    val (maybeStr, maybeQuery) = getQuery(qp)
    maybeQuery match {
      case Left(str) => Left(str)
      case Right(query) => Right((query, qp.copy(query = maybeStr)))
    }
  }

  private def getQuery(qp: QueryParam): (Option[String], Either[String, Query]) = {
    qp.activeQueryTab.getOrElse(defaultActiveQueryTab) match {
      case "#queryUrl" => {
        qp.queryURL match {
          case None => (None, Left(s"No value for queryURL"))
          case Some(queryUrl) => Try {
            val uri = new java.net.URI(queryUrl)
            Source.fromURI(uri).mkString
          }.toEither match {
            case Left(err) => (None,Left(s"Error obtaining data from url $queryUrl: ${err.getMessage} "))
            case Right(str) => (Some(str),Right(str))
          }
        }
      }
      case "#queryFile" => {
        qp.queryFile match {
          case None => (None, Left(s"No value for queryFile"))
          case Some(queryStr) =>
            (Some(queryStr), Right(queryStr))
        }
      }
      case "#queryTextArea" => {
        qp.query match {
          case None => (None, Right(""))
          case Some(query) => {
            (Some(query), Right(query))
          }
        }
      }
      case other => (None, Left(s"Unknown value for activeQueryTab: $other"))
    }
  }

  private def mkQueryParam(partsMap: PartsMap): IO[QueryParam] = for {
    query <- optPartValue("query", partsMap)
    queryURL <- optPartValue("queryURL", partsMap)
    queryFile <- optPartValue("queryFile", partsMap)
    activeQueryTab <- optPartValue("activeQueryTab", partsMap)
  } yield QueryParam(query, queryURL, queryFile, activeQueryTab)


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
    dp.endpoint,
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

  private def dataInfo(rdf: RDFReasoner): Option[Json] = {
    Some(Json.fromFields(
      List(
        ("statements", Json.fromString(rdf.getNumberOfStatements().fold(identity,_.toString))),
        ("nodesPrefixMap", ApiHelper.prefixMap2Json(rdf.getPrefixMap()))
      )
    ))
  }
}