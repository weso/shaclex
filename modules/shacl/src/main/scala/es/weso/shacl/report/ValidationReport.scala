package es.weso.shacl.report

import es.weso.rdf.RDFBuilder
import es.weso.rdf.triples.RDFTriple
import es.weso.shacl.validator.ViolationError
import es.weso.shacl.SHACLPrefixes._
import es.weso.rdf.PREFIXES._

case class ValidationReport(conforms: Boolean,
                            results: Seq[ValidationResult],
                            shapesGraphWellFormed: Boolean
                           ) {

  def toRDF(builder: RDFBuilder): Either[String,RDFBuilder] = {
    val (vr,newVr) = builder.createBNode
    newVr.addType(vr,sh_ValidationReport)
  }

}

object ValidationReport {
  def fromError(e: ViolationError): ValidationReport = {
    ValidationReport(conforms = false, results = Seq(e.toValidationResult), true)
  }
}