package es.weso.shacl

import es.weso.rdf._

trait CoreValidator {
  
  def validate(graph: RDFReader, shapes: Seq[Shape]): Seq[ViolationError] = {
    Seq()
  }
}