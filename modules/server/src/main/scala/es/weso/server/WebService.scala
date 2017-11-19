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

    case req @ GET -> Root / "dataConversions" :?
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
        dataConvert(optData,optDataFormat,optTargetDataFormat)))

    }

    case req @ POST -> Root / "dataConversions" => {

      def cnv(e: Either[String,RDFReasoner],
	            dp: DataParam): Either[String,Option[String]] = for {
        rdf <- e
        str <- rdf.serialize(dp.targetDataFormat.getOrElse(defaultDataFormat))
      } yield Some(str)

      def cb(result: Either[String,RDFReasoner],
	           dp: DataParam,
             partsMap: PartsMap): IO[Response[IO]] = {
       println(s"############### DataParm data: ${dp.data}")
       Ok(html.dataConversions(
          dp.data,
          availableDataFormats,
          dp.dataFormat.getOrElse(defaultDataFormat),
          availableInferenceEngines,
          dp.inference.getOrElse(defaultInference),
          dp.targetDataFormat.getOrElse(defaultDataFormat),
          dp.activeDataTab.getOrElse(defaultActiveDataTab),
          cnv(result,dp)))
      }
     req.decode[Multipart[IO]] { m =>
       val partsMap = parts2Map(m.parts)
       getData(partsMap, cb)
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
      def cb(e: Either[String,RDFReasoner],dp: DataParam, partsMap: PartsMap): IO[Response[IO]] = {
        e.fold(str => BadRequest(str),
          rdf => Ok(html.dataInfo(
            dp.data,
            availableDataFormats,
            dp.dataFormat.getOrElse(defaultDataFormat),
            availableInferenceEngines,
            dp.inference.getOrElse(defaultInference),
            dp.activeDataTab.getOrElse(defaultActiveDataTab))))
      }
      req.decode[Multipart[IO]] { m =>
        val partsMap = parts2Map(m.parts)
        getData(partsMap, cb) }
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

	   def cb(e: Either[String, RDFReasoner],
	         dp: DataParam,
           partsMap: PartsMap): IO[Response[IO]] = {
		 e match {
		  case Left(msg) => BadRequest(s"Error obtaining data: $msg")
		  case Right(rdf) => for {
        optSchema <- optPartValue("schema", partsMap)
        optSchemaFormat <- optPartValue("schemaFormat", partsMap)
        optSchemaEngine <- optPartValue("schemaEngine", partsMap)
        optTriggerMode <- optPartValue("triggerMode", partsMap)
        optShapeMap <- optPartValue("shapeMap", partsMap)
        optSchemaEmbedded <- optPartValue("schemaEmbedded",partsMap)
        schemaEmbedded = optSchemaEmbedded match {
              case Some("true") => true
              case Some("false") => false
              case Some(msg) => defaultSchemaEmbedded
              case None => defaultSchemaEmbedded
            }
        (result, maybeTriggerMode) = validate(dp.data.getOrElse(""), dp.dataFormat,
              optSchema, optSchemaFormat, optSchemaEngine,
              optTriggerMode,
              None, None, optShapeMap,dp.inference)
         triggerMode = maybeTriggerMode.getOrElse(ValidationTrigger.default)
         shapeMap = triggerMode match {
              case TargetDeclarations => None
              case ShapeMapTrigger(sm) => Some(sm.toString)
            }
         response <- Ok(html.validate(
              Some(result),
              dp.data,
              availableDataFormats,
              dp.dataFormat.getOrElse(defaultDataFormat),
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
              dp.inference.getOrElse(defaultInference),
              dp.activeDataTab.getOrElse(defaultActiveDataTab),
              defaultActiveSchemaTab
            ))
		   } yield response
     }
	  }
    req.decode[Multipart[IO]] { m =>
      val partsMap = parts2Map(m.parts)
      getData(partsMap, cb)
    }

    }

    case req@GET -> Root / "validate" :?
      OptExamplesParam(optExamples) +&
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
                                  optInference: Option[String],
                                  dataParam: DataParam,
                                  partsMap: PartsMap,
                                  cb: (Either[String, RDFReasoner], DataParam, PartsMap) => IO[Response[IO]]
                                 ): IO[Response[IO]] = {
    rdf.applyInference(optInference.getOrElse("None")).fold(
      msg => cb(err(s"Error applying inference to RDF: $msg"), dataParam, partsMap),
      (newRdf: RDFReasoner) => cb(Right(newRdf), dataParam, partsMap)
    )
  }

  case class SchemaParam(schema: Option[String],
                         schemaURL: Option[String],
                         schemaFile: Option[String],
                         schemaFormat: Option[String],
                         targetSchemaFormat: Option[String],
                         activeSchemaTab: Option[String]
                        )

  private def mkSchemaParam(partsMap: PartsMap): IO[SchemaParam] = for {
    schema <- optPartValue("schema", partsMap)
    schemaURL <- optPartValue("schemaURL", partsMap)
    schemaFile <- optPartValue("schemaFile", partsMap)
    schemaFormat <- optPartValue("schemaFormat", partsMap)
    targetSchemaFormat <- optPartValue("targetSchemaFormat", partsMap)
    activeSchemaTab <- optPartValue("activeSchemaTab", partsMap)
  } yield SchemaParam(schema,schemaURL,schemaFile,schemaFormat,targetSchemaFormat,activeSchemaTab)

  private def getSchema(partsMap: PartsMap,
                        cb: (Either[String, Schema], SchemaParam, PartsMap) => IO[Response[IO]]
                     ): IO[Response[IO]] = for {
    sp <- mkSchemaParam(partsMap)
    response <- sp.activeSchemaTab.getOrElse(defaultActiveSchemaTab) match {
      case d @"#dataUrl" => cb(err(s"Not implemented"), sp, partsMap)
      case _ => cb(err(s"Not implemented"), sp, partsMap)
    }
  } yield response

  case class DataParam(data: Option[String],
                       dataURL: Option[String],
                       dataFile: Option[String],
                       endpoint: Option[String],
                       dataFormat: Option[String],
                       inference: Option[String],
                       targetDataFormat: Option[String],
                       activeDataTab: Option[String]
                      )

  private def getData(partsMap: PartsMap,
                      cb: (Either[String, RDFReasoner], DataParam, PartsMap) => IO[Response[IO]]
                     ): IO[Response[IO]] = for {
     dp <- mkDataParam(partsMap)
     response <- dp.activeDataTab.getOrElse(defaultActiveDataTab) match {
        case d @"#dataUrl" => {
          dp.dataURL match {
            case None => cb(err(s"Non value for dataURL"),dp, partsMap)
            case Some(dataUrl) => {
              val dataFormat = dp.dataFormat.getOrElse(defaultDataFormat)
              RDFAsJenaModel.fromURI(dataUrl,dataFormat) match {
                case Left(str) => cb(err(s"Error obtaining $dataUrl with $dataFormat: $str"), dp, partsMap)
                case Right(rdf) => {
                  val newDp = dp.copy(data = rdf.serialize(dataFormat).toOption)
                  extendWithInference(rdf,dp.inference, newDp, partsMap, cb)
                }
              }
            }
          }
        }
        case "#dataFile" => {
          dp.dataFile match {
            case None => cb(err(s"No value for dataFile"),dp,partsMap)
            case Some(dataFile) =>
              val dataFormat = dp.dataFormat.getOrElse(defaultDataFormat)
              RDFAsJenaModel.fromChars(dataFile, dataFormat, None) match {
                case Left(msg) => cb(err(msg), dp, partsMap)
                case Right(rdf) => {
                  val newDp = dp.copy(data = Some(dataFile))
                  extendWithInference(rdf, dp.inference, newDp, partsMap, cb)
                }
              }
          }
        }
        case d @ "#dataEndpoint" => {
          dp.endpoint match {
            case None => cb(err(s"No value for endpoint"),dp,partsMap)
            case Some(endpointUrl) => {
              Endpoint.fromString(endpointUrl) match {
                case Left(str) => cb(err(s"Error creating endpoint: $endpointUrl"),dp,partsMap)
                case Right(endpoint) => {
                  extendWithInference(endpoint,dp.inference,dp,partsMap,cb)
                }
              }
            }
          }
        }
        case d @ "#dataTextArea" => {
          dp.data match {
            case None => cb(Right(RDFAsJenaModel.empty),dp,partsMap)
            case Some(data) => {
              RDFAsJenaModel.fromChars(data, dp.dataFormat.getOrElse(defaultDataFormat), None) match {
                case Left(msg) => cb(err(msg),dp, partsMap)
                case Right(rdf) => {
                  extendWithInference(rdf,dp.inference,dp, partsMap, cb)
                }
              }
            }
          }
        }
        case other => cb(err(s"Unknown value for activeDataTab: $other"),dp,partsMap)
      }
    } yield response

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

  def parts2Map(ps: Vector[Part[IO]]): PartsMap = {
    ps.filter(_.name.isDefined).map(p => (p.name.get,p)).toMap
  }

  def optPartValue(key: String, partsMap: PartsMap): IO[Option[String]] = partsMap.get(key) match {
    case Some(part) => part.body.through(utf8Decode).runFoldMonoid.map(Some(_))
    case None => IO(None)
  }

}