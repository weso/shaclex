package es.weso.shacl.report

import es.weso.rdf.RDFBuilder
import es.weso.rdf.saver.RDFSaver
import es.weso.shacl.validator.ViolationError

case class ValidationReport(conforms: Boolean,
                            results: Seq[ValidationResult],
                            shapesGraphWellFormed: Boolean
                           ) extends RDFSaver {

  def toRDF(builder: RDFBuilder): Either[String,RDFBuilder] = {
    Right(ValidationReport2RDF(this,builder))
  }

}

object ValidationReport {
  def fromError(e: ViolationError): ValidationReport = {
    ValidationReport(conforms = false, results = Seq(e.toValidationResult), true)
  }
}