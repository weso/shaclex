package es.weso.shacl.report

import com.typesafe.scalalogging.LazyLogging
import es.weso.rdf.RDFBuilder
import es.weso.rdf.saver.RDFSaver
import es.weso.shacl.SHACLPrefixes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.nodes.{BooleanLiteral, RDFNode, StringLiteral}
import es.weso.shacl.LiteralValue

class ValidationReport2RDF extends RDFSaver with LazyLogging {

  def toRDF(vr: ValidationReport, initial: RDFBuilder): RDFBuilder = {
    val result = validationReport(vr).run(initial)
    result.value._1
  }

  private def validationReport(vr: ValidationReport): RDFSaver[Unit] = for {
    _ <- addPrefix("sh", sh)
    node <- createBNode
    _ <- addTriple(node, `rdf:type`, `sh:ValidationReport`)
    _ <- addTriple(node, sh_conforms, BooleanLiteral(vr.conforms))
    _ <- results(node, vr.results)
  } yield ()

  private def results(id: RDFNode, ts: Seq[AbstractResult]): RDFSaver[Unit] =
    saveList(ts.toList, result(id))

  private def result(id: RDFNode)(ar: AbstractResult): RDFSaver[Unit] = ar match {
    case vr: ValidationResult =>
      for {
        node <- createBNode()
        _    <- addTriple(id, sh_result, node)
        _    <- addTriple(node, `rdf:type`, `sh:ValidationResult`)
        _    <- addTriple(node, sh_resultSeverity, vr.resultSeverity.toIRI)
        _    <- addTriple(node, sh_focusNode, vr.focusNode)
        _    <- addTriple(node, sh_sourceConstraintComponent, vr.sourceConstraintComponent)
        _    <- addTriple(node, sh_sourceShape, vr.sourceShape.id)
        _    <- addTripleObjects(node, sh_value, vr.values.toList)
        _    <- addTripleObjects(node, sh_resultMessage, vr.messageMap.getRDFNodes)
        _    <- saveList(vr.message.toList, message(node))
        _ <- vr.focusPath match {
          case None => ok(())
          case Some(path) =>
            for {
              path <- makePath(path)
              _    <- addTriple(node, sh_resultPath, path)
            } yield ()
        }
      } yield ()
    case mr: MsgError => for {
     node <- createBNode()
     _ <- addTriple(node,sh_resultMessage, StringLiteral(mr.msg))
    } yield ()
  }

  private def message(node: RDFNode)(msg: LiteralValue): RDFSaver[Unit] = for {
    _ <- addTriple(node,sh_message,msg.literal)
  } yield ()

}

object ValidationReport2RDF {

  def apply(vr: ValidationReport, builder: RDFBuilder): RDFBuilder =
    new ValidationReport2RDF().toRDF(vr,builder)

}