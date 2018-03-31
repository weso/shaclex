package es.weso.shacl.report

import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.RDFBuilder
import es.weso.rdf.saver.RDFSaver
import es.weso.shacl.SHACLPrefixes._
import es.weso.rdf.PREFIXES.{sh => _, _}
import es.weso.rdf.nodes.{BooleanLiteral, RDFNode, StringLiteral}

class ValidationReport2RDF extends RDFSaver with LazyLogging {

  def toRDF(vr: ValidationReport, initial: RDFBuilder): RDFBuilder = {
    val result = validationReport(vr).run(initial)
    result.value._1
  }

  private def validationReport(vr: ValidationReport): RDFSaver[Unit] = for {
    _ <- addPrefix("sh", sh.str)
    node <- createBNode
    _ <- addTriple(node, rdf_type, sh_ValidationReport)
    _ <- addTriple(node, sh_conforms, BooleanLiteral(vr.conforms))
    _ <- results(node, vr.results)
  } yield ()

  private def results(id: RDFNode, ts: Seq[ValidationResult]): RDFSaver[Unit] =
    saveList(ts.toList, result(id))

  private def result(id: RDFNode)(vr: ValidationResult): RDFSaver[Unit] = for {
    node <- createBNode()
    _ <- addTriple(id, sh_result, node)
    _ <- addTriple(node, rdf_type, sh_ValidationResult)
    _ <- addTriple(node, sh_resultSeverity, vr.resultSeverity.toIRI)
    _ <- addTriple(node, sh_focusNode, vr.focusNode)
    _ <- addTriple(node, sh_sourceConstraintComponent, vr.sourceConstraintComponent)
//    _ <- addTriple(node, sh_message, StringLiteral(vr.message.headOption.getOrElse("")))
  } yield ()

}

object ValidationReport2RDF {

  def apply(vr: ValidationReport, builder: RDFBuilder): RDFBuilder =
    new ValidationReport2RDF().toRDF(vr,builder)

}