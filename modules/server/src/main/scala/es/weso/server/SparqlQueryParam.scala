package es.weso.server

import cats.effect.IO
import Defaults._

import scala.io.Source
import scala.util.Try

case class SparqlQueryParam(query: Option[Query],
                      queryURL: Option[String],
                      queryFile: Option[String],
                      activeQueryTab: Option[String]
                     ) {

  def getQuery: (Option[String], Either[String, Query]) = {
    activeQueryTab.getOrElse(defaultActiveQueryTab) match {
      case "#queryUrl" => {
        queryURL match {
          case None => (None, Left(s"No value for queryURL"))
          case Some(queryUrl) => Try {
            val uri = new java.net.URI(queryUrl)
            Source.fromURI(uri).mkString
          }.toEither match {
            case Left(err) => (None, Left(s"Error obtaining data from url $queryUrl: ${err.getMessage} "))
            case Right(str) => (Some(str), Right(Query(str)))
          }
        }
      }
      case "#queryFile" => {
        queryFile match {
          case None => (None, Left(s"No value for queryFile"))
          case Some(queryStr) =>
            (Some(queryStr), Right(Query(queryStr)))
        }
      }
      case "#queryTextArea" => {
        query match {
          case None => (None, Right(Query("")))
          case Some(query) => {
            (Some(query.str), Right(query))
          }
        }
      }
      case other => (None, Left(s"Unknown value for activeQueryTab: $other"))
    }
  }

}

object SparqlQueryParam {

  private[server] def mkQuery(partsMap: PartsMap
                     ): IO[Either[String, (Query, SparqlQueryParam)]] = for {
    qp <- mkQueryParam(partsMap)
  } yield {
    val (maybeStr, maybeQuery) = qp.getQuery
    maybeQuery match {
      case Left(str) => Left(str)
      case Right(query) => Right((query, qp.copy(query = Some(query))))
    }
  }

  private[server] def mkQueryParam(partsMap: PartsMap): IO[SparqlQueryParam] = for {
    queryStr <- partsMap.optPartValue("query")
    queryURL <- partsMap.optPartValue("queryURL")
    queryFile <- partsMap.optPartValue("queryFile")
    activeQueryTab <- partsMap.optPartValue("activeQueryTab")
  } yield SparqlQueryParam(queryStr.map(Query(_)), queryURL, queryFile, activeQueryTab)

}