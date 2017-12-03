package es.weso.server

import java.io.ByteArrayOutputStream

import cats.implicits._
import cats.effect.IO
import es.weso.rdf.{PrefixMap, RDFReasoner}
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.schema.{DataFormats, Result, Schemas, ValidationTrigger}
import es.weso.utils.FileUtils
import io.circe._
import io.circe.parser._
import org.apache.jena.query.{Query, QueryExecutionFactory, ResultSetFormatter}
import org.http4s.Uri
import org.http4s.client.blaze.PooledHttp1Client

import scala.util.Try

object ApiHelper {

  def prefixMap2Json(pm: PrefixMap): Json = {
    Json.fromFields(pm.pm.map { case (prefix, iri) => (prefix.str, Json.fromString(iri.getLexicalForm)) })
  }

  def resolveUri(baseUri: Uri, urlStr: String): Either[String, Option[String]] = {
    // TODO: handle timeouts
    Uri.fromString(urlStr).fold(
      fail => {
        println(s"Error parsing $urlStr")
        Left(fail.message)
      },
      uri => Try {
        val httpClient = PooledHttp1Client[IO]()
        val resolvedUri = baseUri.resolve(uri)
        println(s"Resolved: $resolvedUri")
        httpClient.expect[String](resolvedUri).unsafeRunSync()
      }.toEither.leftMap(_.getMessage).map(Some(_))
    )
  }

  def dataConvert(optData: Option[String],
                  optDataFormat: Option[String],
                  optTargetDataFormat: Option[String]): Either[String, Option[String]] = optData match {
    case None => Right(None)
    case Some(data) => {
      val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
      val resultDataFormat = optTargetDataFormat.getOrElse(DataFormats.defaultFormatName)
      for {
        rdf <- RDFAsJenaModel.fromChars(data, dataFormat, None)
        str <- rdf.serialize(resultDataFormat)
      } yield Some(str)
    }
  }

  def validate(data: String,
               optDataFormat: Option[String],
               optSchema: Option[String],
               optSchemaFormat: Option[String],
               optSchemaEngine: Option[String],
               optTriggerMode: Option[String],
               optNode: Option[String],
               optShape: Option[String],
               optShapeMap: Option[String],
               optInference: Option[String]
              ): (Result, Option[ValidationTrigger]) = {

    val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
    val schemaEngine = optSchemaEngine.getOrElse(Schemas.defaultSchemaName)
    val schemaFormat = optSchema match {
      case None => dataFormat
      case Some(_) => optSchemaFormat.getOrElse(Schemas.defaultSchemaFormat)
    }
    val schemaStr = optSchema match {
      case None => data
      case Some(schema) => schema
    }

    val base = Some(FileUtils.currentFolderURL)

    Schemas.fromString(schemaStr, schemaFormat, schemaEngine, base) match {
      case Left(e) =>
        (Result.errStr(s"Error reading schema: $e\nschemaFormat: $schemaFormat, schemaEngine: $schemaEngine\nschema:\n$schemaStr"), None)
      case Right(schema) => {
        RDFAsJenaModel.fromChars(data, dataFormat, base) match {
          case Left(e) =>
            (Result.errStr(s"Error reading rdf data: $e\ndataFormat: $dataFormat\nRDF Data:\n$data"), None)
          case Right(rdf) => {
            rdf.applyInference(optInference.getOrElse("None")) match {
              case Left(msg) => (Result.errStr(s"Error applying inference to RDF: $msg"), None)
              case Right(newRdf) => {
                val triggerMode = optTriggerMode.getOrElse(ValidationTrigger.default.name)
                val shapeMap = optShapeMap.getOrElse("")
                ValidationTrigger.findTrigger(triggerMode, shapeMap, base, optNode, optShape, rdf.getPrefixMap, schema.pm) match {
                  case Left(msg) => (
                    Result.errStr(s"Cannot obtain trigger: $triggerMode\nshapeMap: $shapeMap\nmsg: $msg"),
                    None)
                  case Right(trigger) => (schema.validate(newRdf, trigger), Some(trigger))
                }
              }
            }
          }
        }
      }
    }
  }


  def query(data: String,
            optDataFormat: Option[String],
            optQuery: Option[String],
            optInference: Option[String]
           ): Either[String, Json] = {
    optQuery match {
      case None => Right(Json.Null)
      case Some(queryStr) => {
        val dataFormat = optDataFormat.getOrElse(DataFormats.defaultFormatName)
        val base = Some(FileUtils.currentFolderURL)
        for {
          basicRdf <- RDFAsJenaModel.fromChars(data, dataFormat, base)
          rdf <- basicRdf.applyInference(optInference.getOrElse("None"))
          json <- rdf.queryAsJson(queryStr)
        } yield json
      }
    }

  }

}