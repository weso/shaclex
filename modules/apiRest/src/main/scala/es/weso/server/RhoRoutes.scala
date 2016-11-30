package es.weso.server
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.{DataFormats, Schemas, ValidationTrigger}
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

  "Available trigger modes" **
    GET / api / "schema/triggerModes" |>> { () =>
    val triggerModes = ValidationTrigger.triggerValues.map(_._1)
    val json = Json.fromValues(triggerModes.map(Json.fromString(_)))
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
    param[String]("schemaEngine") &
    param[String]("triggerMode") &
    param[String]("node") &
    param[String]("shape") |>> {
     (data: String,
      dataFormat: String,
      schema: String,
      schemaFormat: String,
      schemaEngine: String,
      triggerMode: String,
      node: String,
      shape: String
     ) =>
    validate(data,dataFormat,
      schema,schemaFormat,schemaEngine,
      triggerMode,node,shape)
  }

/*  "GET Validate" **
    GET / api / "validate" +?
      param[String]("data") &
      param[String]("dataFormat") &
      param[String]("schema") &
      param[String]("schemaFormat") &
      param[String]("schemaEngine") &
      param[String]("triggerMode") &
      param[String]("node") &
      param[String]("shape")|>> {
       (data: String,
        dataFormat: String,
        schema: String,
        schemaFormat: String,
        schemaEngine: String,
        triggerMode: String,
        node: String,
        shape: String
    ) =>
    validate(data,dataFormat,
      schema,schemaFormat,schemaEngine,
      triggerMode, node, shape)
  } */

  "Convert RDF Data" **
    GET / api / "data/convert" +?
    param[String]("data") &
    param[String]("dataFormat") &
    param[String]("resultFormat") |>> {
    (data: String,
     dataFormat: String,
     resultFormat: String
    ) =>
      convertData(data,dataFormat,resultFormat)
  }

  "Convert Schema" **
    GET / api / "schema/convert" +?
    param[String]("schema") &
    param[String]("schemaFormat") &
    param[String]("schemaEngine") &
    param[String]("resultFormat") &
    param[String]("resultEngine") |>> {
    (schema: String,
     schemaFormat: String,
     schemaEngine: String,
     resultFormat: String,
     resultEngine: String
    ) =>
      convertSchema(schema,
        schemaFormat,schemaEngine,
        resultFormat,resultEngine)
  }

  def convertData(
     data: String,
     dataFormat: String,
     resultFormat: String
    ) = {
     RDFAsJenaModel.fromChars(data, dataFormat, None) match {
       case Failure(e) => BadRequest(s"Error reading rdf: $e\nRdf string: $data")
       case Success(rdf) => {
         val result = rdf.serialize(resultFormat)
         Ok(result)
       }
  }
  }

  def convertSchema(
    schema: String,
    schemaFormat: String,
    schemaEngine: String,
    resultFormat: String,
    resultEngine: String
   ) = {
     Schemas.fromString(schema, schemaFormat, schemaEngine, None) match {
       case Failure(e) => BadRequest(s"Error reading schema: $e\n\nFormat: $schemaFormat\nEngine: $schemaEngine\nSchema string: $schema")
       case Success(schema) => {
         if (resultEngine == schemaEngine) {
           schema.serialize(resultFormat) match {
             case Failure(err) => BadRequest(s"Error serializing schema to $resultFormat: $err")
             case Success(result) => Ok(result)
           }
         } else {
           BadRequest(s"Conversion between engines not supported yet")
         }
       }
         }
   }

  def validate(data: String,
               dataFormat: String,
               schema: String,
               schemaFormat: String,
               schemaEngine: String,
               triggerMode: String,
               maybeNode: String,
               maybeShape: String
              ) = {
    Schemas.fromString(schema, schemaFormat, schemaEngine, None) match {
      case Failure(e) => BadRequest(s"Error reading schema: $e\nString: $schema")
      case Success(schema) => {
        RDFAsJenaModel.fromChars(data, dataFormat, None) match {
          case Failure(e) => BadRequest(s"Error reading rdf: $e\nRdf string: $data")
          case Success(rdf) => {
            val node =
              if (maybeNode == "") None
              else Some(maybeNode)
            val shape =
              if (maybeShape == "") None
              else Some(maybeShape)
            ValidationTrigger.findTrigger(triggerMode,node,shape,schema.pm) match {
              case Left(err) => BadRequest(err)
              case Right(trigger) => {
                val result = schema.validateWithTrigger(rdf,trigger)
                val jsonResult = result.toJsonString2spaces
                Ok(jsonResult)
              }
            }
          }
        }
      }
    }
  }


}