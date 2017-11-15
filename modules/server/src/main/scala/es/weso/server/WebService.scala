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
import es.weso.rdf._
import cats.effect.IO._

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
           form: UrlForm): IO[Response[IO]] = {
       Ok(html.dataConversions(
          dp.data,
          availableDataFormats,
          dp.dataFormat.getOrElse(defaultDataFormat),
          availableInferenceEngines,
          dp.inference.getOrElse(defaultInference),
          dp.targetDataFormat.getOrElse(defaultDataFormat),
          dp.activeDataTab,
          cnv(result,dp)))
      }
     req.decode[UrlForm] { form => getData(form, cb) }
      /*maybeData =>
      ))) */
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
      def cb(e: Either[String,RDFReasoner],dp: DataParam, form: UrlForm): IO[Response[IO]] =
        e.fold(str => BadRequest(str),
               rdf => Ok(html.dataInfo(
            dp.data,
            availableDataFormats,
            dp.dataFormat.getOrElse(defaultDataFormat),
            availableInferenceEngines,
            dp.inference.getOrElse(defaultInference),
            dp.activeDataTab)))
      req.decode[UrlForm] { getData(_, cb) }
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
           form: UrlForm): IO[Response[IO]] = {
		e match {
		  case Left(msg) => BadRequest(s"Error obtaining data: $msg")
		  case Right(rdf) => {
  	    val values = form.values
  			logger.info(s"################## POST validate:\n${values.mkString("\n")}")
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
			val (result, maybeTriggerMode) = validate(data, optDataFormat,
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
              dp.activeDataTab
            ))
		   }
        }
	  }
    req.decode[UrlForm] { getData(_,cb) }
	  
/*      req.decode[UrlForm] { m => {
        val values = m.values
        logger.info(s"################## POST data:\n${m.values.mkString("\n")}")
        values.get("rdfDataActiveTab").map(_.head) match {
          case Some("#dataUrl") => {
            values.get("dataURL").map(_.head) match {
              case None => BadRequest(s"Non value for dataUrl")
              case Some(dataUrl) => {
                val optDataFormat = values.get("dataFormat").map(_.head)
                BadRequest(s"Non implemented RDF from URL: $dataUrl. Format: $optDataFormat")
              }
            }
          }
          case Some("#dataFile") => {
            values.get("dataFile").map(_.head) match {
              case None => BadRequest(s"No value for dataFile")
              case Some(dataFile) => BadRequest(s"Not implemented value from dataFile $dataFile")
            }
          }
          case Some("#dataEndpoint") => {
            values.get("endpoint").map(_.head) match {
              case None => BadRequest(s"No value for endpoint")
              case Some(endpoint) => BadRequest(s"Not implemented data from endpoint yet")
            }
          }
          case Some("#dataTextArea") => {
            val data = values.get("data").map(_.head).getOrElse("")
            val optSchema = values.get("schema").map(_.head)
            val optDataFormat = values.get("dataFormat").map(_.head)
            val optSchemaFormat = values.get("schemaFormat").map(_.head)
            val optSchemaEngine = values.get("schemaEngine").map(_.head)
            val optTriggerMode = values.get("triggerMode").map(_.head)
            val optShapeMap = values.get("shapeMap").map(_.head)
            val optInference = values.get("inference").map(_.head)
            val optSchemaEmbedded = values.get("schemaEmbedded").map(_.head)
            val optActiveDataTab = values.get("inputRdfDataActive").map(_.head)

            val schemaEmbedded = optSchemaEmbedded match {
              case Some("true") => true
              case Some("false") => false
              case Some(msg) => {
                logger.info(s"Unsupported value for schema embedded: $msg")
                defaultSchemaEmbedded
              }
              case None => defaultSchemaEmbedded
            }

            val (result, maybeTriggerMode) = validate(data, optDataFormat,
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
              optInference.getOrElse(defaultInference),
              optActiveDataTab.getOrElse(defaultActiveDataTab)
            ))
          }
          case other => BadRequest(s"Unknown value for rdfDataTabActive: $other")
        }
      }
      } */
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
              optActiveDataTab.getOrElse(defaultActiveDataTab)
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
                                  form: UrlForm,
                                  cb: (Either[String, RDFReasoner], DataParam, UrlForm) => IO[Response[IO]]
                                 ): IO[Response[IO]] = {
    rdf.applyInference(optInference.getOrElse("None")).fold(
      msg => cb(err(s"Error applying inference to RDF: $msg"), dataParam, form),
      (newRdf: RDFReasoner) => cb(Right(newRdf), dataParam, form)
    )
  }

  case class DataParam(data: Option[String],
                       dataURL: Option[String],
                       endpoint: Option[String],
                       dataFormat: Option[String],
                       inference: Option[String],
                       targetDataFormat: Option[String],
                       activeDataTab: String)

  private def getData(form: UrlForm,
                      cb: (Either[String, RDFReasoner], DataParam, UrlForm) => IO[Response[IO]]
                     ): IO[Response[IO]] = {
     val values = form.values
     val dp = mkDataParam(values)
     println(s"###################### POST (getData): ${values.mkString("\n")}")
	   val activeDataTab = values.get("rdfDataActiveTab").map(_.head).getOrElse(defaultActiveDataTab)
      activeDataTab match {
        case d @"#dataUrl" => {
          values.get("dataURL").map(_.head) match {
            case None => cb(err(s"Non value for dataURL"),dp, form)
            case Some(dataUrl) => {
              RDFAsJenaModel.fromURI(dataUrl,dp.dataFormat.getOrElse(defaultDataFormat)) match {
                case Left(str) => cb(err(str),dp, form)
                case Right(rdf) => {
                  extendWithInference(rdf,dp.inference,dp,form,cb)
                }
              }
            }
          }
        }
        case "#dataFile" => {
          values.get("dataFile").map(_.head) match {
            case None => cb(err(s"No value for dataFile"),dp,form)
            case Some(dataFile) => cb(err(s"Not implemented value from dataFile $dataFile"),dp,form)
          }
        }
        case d @ "#dataEndpoint" => {
          values.get("endpoint").map(_.head) match {
            case None => cb(err(s"No value for endpoint"),dp,form)
            case Some(endpointUrl) => {
              Endpoint.fromString(endpointUrl) match {
                case Left(str) => cb(err(s"Error creating endpoint: $endpointUrl"),dp,form)
                case Right(endpoint) => {
                  extendWithInference(endpoint,dp.inference,dp,form,cb)
                }
              }
            }
          }
        }
        case d @ "#dataTextArea" => {
          values.get("data").map(_.head) match {
            case None => cb(Right(RDFAsJenaModel.empty),dp,form)
            case Some(data) => {
              RDFAsJenaModel.fromChars(data, dp.dataFormat.getOrElse(defaultDataFormat), None) match {
                case Left(msg) => cb(err(msg),dp, form)
                case Right(rdf) => {
                  extendWithInference(rdf,dp.inference,dp, form, cb)
                }
              }
            }
          }
        }
        case other => cb(err(s"Unknown value for activeDataTab: $other"),dp,form)
      }
    }

  private def mkDataParam(values: Map[String,Seq[String]]): DataParam = {
    val data = values.get("data").map(_.head)
    val dataURL = values.get("dataURL").map(_.head)
    val endpoint = values.get("endpoint").map(_.head)
    val dataFormat = values.get("dataFormat").map(_.head)
    val inference = values.get("inference").map(_.head)
    val targetDataFormat = values.get("targetDataFormat").map(_.head)
    val activeDataTab = values.get("activeDataTab").map(_.head).getOrElse(defaultActiveDataTab)
    DataParam(data,dataURL,endpoint,dataFormat,inference,targetDataFormat,activeDataTab)
  }
}