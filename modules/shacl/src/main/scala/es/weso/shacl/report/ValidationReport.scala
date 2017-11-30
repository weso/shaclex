package es.weso.shacl.report

case class ValidationReport(conforms: Boolean,
                            results: Seq[ValidationResult],
                            shapesGraphWellFormed: Boolean
                           )

