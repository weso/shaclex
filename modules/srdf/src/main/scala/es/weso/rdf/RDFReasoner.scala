package es.weso.rdf

trait RDFReasoner extends RDFReader {

  type Rdf <: RDFReasoner

  def applyInference(inference: String): Either[String, RDFReasoner]

  def availableInferenceEngines: List[String]

}

