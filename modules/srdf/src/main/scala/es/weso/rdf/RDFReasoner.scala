package es.weso.rdf

import es.weso.rdf.triples._
import scala.util.Try
import es.weso.rdf.nodes._
import PREFIXES._

trait RDFReasoner {

  type Rdf <: RDFReasoner

  def applyInference(inference: String): Either[String, Rdf]

  def availableInferenceEngines: List[String]

}

