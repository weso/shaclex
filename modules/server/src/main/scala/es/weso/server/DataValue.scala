package es.weso.server

case class DataValue(data: Option[String],
                     dataURL: Option[String],
                     currentDataFormat: String,
                     availableDataFormats: List[String],
                     activeDataTab: String
                    )