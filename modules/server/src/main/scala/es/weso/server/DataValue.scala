package es.weso.server

case class DataValue(data: Option[String],
                     dataURL: Option[String],
                     currentDataFormat: String,
                     availableDataFormats: List[String],
                     currentInferenceEngine: String,
                     availableInferenceEngines: List[String],
                     endpoint: Option[String],
                     activeDataTab: String
                    )