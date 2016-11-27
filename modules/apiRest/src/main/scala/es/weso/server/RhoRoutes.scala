package es.weso.server
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.{DataFormats, Schemas}
import org.http4s.dsl.{BadRequest, Ok, OptionalQueryParamDecoderMatcher, QueryParamDecoderMatcher}
import org.http4s.rho.{param, _}
import org.http4s.twirl._
import org.http4s.rho.swagger._
import io.circe.Json
import org.http4s.{EntityEncoder, UrlForm}
import org.http4s.circe._

import scala.util.{Failure, Success}


class RhoRoutes extends RhoService {

  // Added this line for compatibility with scala 2.11.8
  // Following this issue: https://github.com/http4s/rho/issues/136
  implicitly[EntityEncoder[String]]

  val api = "api/v1"

  // This is needed for the routes that return json4s `JValues`
  object DataParam extends QueryParamDecoderMatcher[String]("data")
  object DataFormatParam extends OptionalQueryParamDecoderMatcher[String]("dataFormat")
  object SchemaParam extends QueryParamDecoderMatcher[String]("schema")
  object SchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("schemaFormat")
  object SchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("schemaEngine")

  "Available schema engines" **
    GET / api / "schema/engines" |>> { () =>
     val engines = Schemas.availableSchemaNames
     val json = Json.fromValues(engines.map(str => Json.fromString(str)))
     Ok(json)
  }

  "Default schema engine" **
    GET / api / "schema/engines/default" |>> { () =>
    val schemaEngine = Schemas.defaultSchemaName
    Ok(schemaEngine)
  }

  "Available schema formats" **
    GET / api / "schema/formats" |>> { () =>
    val formats = Schemas.availableFormats
    val json = Json.fromValues(formats.map(str => Json.fromString(str)))
    Ok(json)
  }

  "Available data formats" **
    GET / api / "data/formats" |>> { () =>
    val formats = DataFormats.formatNames
    val json = Json.fromValues(formats.map(str => Json.fromString(str)))
    Ok(json)
  }

  "Default data format" **
    GET / api / "schema/engines/default" |>> { () =>
    val schemaEngine = Schemas.defaultSchemaName
    Ok(schemaEngine)
  }

  "POST Validate" **
    POST / "validate" +?
    param[String]("data") &
    param[String]("dataFormat") &
    param[String]("schema") &
    param[String]("schemaFormat") &
    param[String]("schemaEngine") |>> {
     (data: String,
      dataFormat: String,
      schema: String,
      schemaFormat: String,
      schemaEngine: String
      ) =>
    validate(data,dataFormat,schema,schemaFormat,schemaEngine)
  }

  "GET Validate" **
    GET / api / "validate" +?
      param[String]("data") &
      param[String]("dataFormat") &
      param[String]("schema") &
      param[String]("schemaFormat") &
      param[String]("schemaEngine") |>> {
       (data: String,
        dataFormat: String,
        schema: String,
        schemaFormat: String,
        schemaEngine: String
       ) =>
    validate(data,dataFormat,schema,schemaFormat,schemaEngine)
  }

  def validate(data: String,
               dataFormat: String,
               schema: String,
               schemaFormat: String,
               schemaEngine: String) = {
    Schemas.fromString(schema, schemaFormat, schemaEngine, None) match {
      case Failure(e) => BadRequest(s"Error reading schema: $e\nString: $schema")
      case Success(schema) => {
        RDFAsJenaModel.fromChars(data, dataFormat, None) match {
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