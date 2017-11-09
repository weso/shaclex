package es.weso.rdf

trait RDFReasoner {

  type Rdf <: RDFReasoner

  def applyInference(inference: String): Either[String, Rdf]

  def availableInferenceEngines: List[String]

}

