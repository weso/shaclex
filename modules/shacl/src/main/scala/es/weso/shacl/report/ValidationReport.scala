package es.weso.shacl.report

import es.weso.rdf.RDFBuilder
import es.weso.rdf.saver.RDFSaver

case class ValidationReport(conforms: Boolean,
                            results: Seq[AbstractResult],
                            shapesGraphWellFormed: Boolean
                           ) extends RDFSaver {

  def toRDF(builder: RDFBuilder): Either[String,RDFBuilder] = {
    Right(ValidationReport2RDF(this,builder))
  }

}

object ValidationReport {
  def fromError(e: AbstractResult): ValidationReport = {
    ValidationReport(conforms = false, results = Seq(e), true)
  }
}