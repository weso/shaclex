package es.weso.server

import Defaults._
import cats.effect.IO
import es.weso.rdf.RDFReasoner
import es.weso.rdf.jena.{Endpoint, RDFAsJenaModel}

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

  def parseDataTab(tab: String): Either[String, DataInputType] = {
    println(s"parseDataTab: tab = $tab")
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

  private def applyInference(rdf: RDFReasoner, inference: Option[String], dataFormat: String): (Option[String],Either[String,RDFReasoner]) = {
    extendWithInference(rdf, inference) match {
      case Left(msg) => (rdf.serialize(dataFormat).toOption, Left(s"Error applying inference: $msg"))
      case Right(newRdf) => (newRdf.serialize(dataFormat).toOption, Right(newRdf))
    }
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

  def getData: (Option[String], Either[String,RDFReasoner]) = {
    println(s"ActiveDataTab: $activeDataTab")
    val inputType = activeDataTab match {
      case None => {
        if (endpoint.isDefined) Right(dataEndpointType)
        else Right(dataTextAreaType)
      }
      case Some(a) => parseDataTab(a)
    }
    println(s"Input type: $inputType")
    inputType match {
      case Right(`dataUrlType`) => {
        dataURL match {
          case None => (None, Left(s"Non value for dataURL"))
          case Some(dataUrl) => {
            val dataFormat = dataFormatUrl.getOrElse(defaultDataFormat)
            RDFAsJenaModel.fromURI(dataUrl, dataFormat) match {
              case Left(str) => (None, Left(s"Error obtaining $dataUrl with $dataFormat: $str"))
              case Right(rdf) => applyInference(rdf, inference, dataFormat)
            }
          }
        }
      }
      case Right(`dataFileType`) => {
        dataFile match {
          case None => (None, Left(s"No value for dataFile"))
          case Some(dataStr) =>
            val dataFormat = dataFormatFile.getOrElse(defaultDataFormat)
            RDFAsJenaModel.fromChars(dataStr, dataFormat, None) match {
              case Left(msg) => (Some(dataStr), Left(msg))
              case Right(rdf) => {
                extendWithInference(rdf, inference) match {
                  case Left(msg) => (rdf.serialize(dataFormat).toOption, Left(s"Error applying inference: $msg"))
                  case Right(newRdf) => (newRdf.serialize(dataFormat).toOption, Right(newRdf))
                }
              }
            }
        }
      }
      case Right(`dataEndpointType`) => {
        endpoint match {
          case None => (None, Left(s"No value for endpoint"))
          case Some(endpointUrl) => {
            Endpoint.fromString(endpointUrl) match {
              case Left(str) => (None, Left(s"Error creating endpoint: $endpointUrl"))
              case Right(endpoint) => {
                (None, extendWithInference(endpoint, inference))
              }
            }
          }
        }
      }
      case Right(`dataTextAreaType`) => {
        data match {
          case None => (None, Right(RDFAsJenaModel.empty))
          case Some(data) => {
            val dataFormat = dataFormatTextarea.getOrElse(defaultDataFormat)
            RDFAsJenaModel.fromChars(data, dataFormat, None) match {
              case Left(msg) => (Some(data), Left(msg))
              case Right(rdf) => {
                extendWithInference(rdf, inference) match {
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

}

object DataParam {

  private[server] def mkData(partsMap: PartsMap): IO[Either[String, (RDFReasoner, DataParam)]] = for {
    dp <- mkDataParam(partsMap)
  } yield {
    val (maybeStr, maybeData) = dp.getData
    maybeData match {
      case Left(str) => Left(str)
      case Right(data) => Right((data, dp.copy(data = maybeStr)))
    }
  }


  private[server] def mkDataParam(partsMap: PartsMap): IO[DataParam] = for {
    data <- partsMap.optPartValue("data")
    dataURL <- partsMap.optPartValue("dataURL")
    dataFile <- partsMap.optPartValue("dataFile")
    endpoint <- partsMap.optPartValue("endpoint")
    dataFormatTextArea <- partsMap.optPartValue("dataFormatTextArea")
    dataFormatUrl <- partsMap.optPartValue("dataFormatUrl")
    dataFormatFile <- partsMap.optPartValue("dataFormatFile")
    inference <- partsMap.optPartValue("inference")
    targetDataFormat <- partsMap.optPartValue("targetDataFormat")
    activeDataTab <- partsMap.optPartValue("rdfDataActiveTab")
  } yield {
    println(s"<<<***Data: $data")
    println(s"<<<***Data Format TextArea: $dataFormatTextArea")
    println(s"<<<***Data Format Url: $dataFormatUrl")
    println(s"<<<***Data Format File: $dataFormatFile")
    println(s"<<<***Data URL: $dataURL")
    println(s"<<<***Endpoint: $endpoint")
    println(s"<<<***ActiveDataTab: $activeDataTab")
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

}