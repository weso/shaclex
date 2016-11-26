package es.weso.server
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.Schemas
import org.http4s.dsl.{BadRequest, Ok, OptionalQueryParamDecoderMatcher, QueryParamDecoderMatcher}
import org.http4s.rho.{param, _}
import org.http4s.twirl._
import org.http4s.rho.swagger._

import scala.util.{Failure, Success}


class RhoRoutes extends RhoService {
  // This is needed for the routes that return json4s `JValues`
  object DataParam extends QueryParamDecoderMatcher[String]("data")
  object DataFormatParam extends OptionalQueryParamDecoderMatcher[String]("dataFormat")
  object SchemaParam extends QueryParamDecoderMatcher[String]("schema")
  object SchemaFormatParam extends OptionalQueryParamDecoderMatcher[String]("schemaFormat")
  object SchemaEngineParam extends OptionalQueryParamDecoderMatcher[String]("schemaEngine")

  "Just a friendly hello route" **
    GET / "hello" |>> { () => Ok("Hello world!") }

  "Validate" **
    GET / "validate" +?
      param[String]("data") &
      param[String]("schema") |>> { (data: String, schema: String) => {
    val schemaFormat = "SHEXC"
    val dataFormat = "TURTLE"
    val schemaEngine = "SHEX"
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
}