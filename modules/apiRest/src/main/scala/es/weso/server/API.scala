package es.weso.server

import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.{DataFormats, Schemas}
import org.http4s.HttpService
import org.http4s.MediaType.`text/html`
import org.http4s.dsl.{+&, ->, /, :?, BadRequest, GET, Ok, OptionalQueryParamDecoderMatcher, POST, QueryParamDecoderMatcher, Root}
import org.http4s.headers.`Content-Type`
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.{Server, ServerApp}

import scala.util.{Failure, Success}
import scalaz.concurrent.Task

/**
  * Created by Labra on 26/11/2016.
  */
/*
object API extends ServerApp {

  val API = "api/v1"

  object DataParam extends QueryParamDecoderMatcher[String]("data")
  object DataFormatParam extends OptionalQueryParamDecoderMatcher[String]("dataFormat")
  object SchemaParam extends QueryParamDecoderMatcher[String]("schema")
  object SchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("schemaFormat")
  object SchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("schemaEngine")

  val service = HttpService {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
/*    case GET -> Root / "index" =>
      Ok(html.index()) */

    case request @ GET -> Root => {
      val str =
        """|<html><head>Validation</head>
           |<body>
           |<form action="http://localhost:8080/api/v1/validate" method="get">
           |<textarea rows="20" cols="70" name="schema" placeholder="schema"></textarea>
           |<input name="schemaEngine" value="SHEX" />
           |<input name="schemaFormat" value="SHEXC" />
           |<textarea rows="20" cols="70" name="data" placeholder="data"></textarea>
           |<input name="dataFormat" value="TURTLE" />
           |<button type="submit">Validate</button>
           |</form>
           |</body>
        """.stripMargin
      Ok(str).
        withContentType(Some(`Content-Type`(`text/html`)))
    }
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
  }

  override def server(args: List[String]): Task[Server] = {
    BlazeBuilder
      .bindHttp(8080, "localhost")
      .mountService(service, "/")
      .start
  }

}
*/