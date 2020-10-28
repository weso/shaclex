package es.weso.schema

import es.weso.rdf.RDFBuilder
import es.weso.shacl.report.ValidationReport
import cats.effect._
import cats.implicits._
import es.weso.rdf.jena.RDFAsJenaModel
import org.apache.jena.rdf.model.Model

sealed abstract class RDFReport extends Product with Serializable {
  def toRDF(builder: RDFBuilder): IO[RDFBuilder]
}

case object EmptyReport extends RDFReport {
    override def toRDF(builder: RDFBuilder) = builder.pure[IO]
}

case class ShaclexReport(vr: ValidationReport) extends RDFReport {
    override def toRDF(builder: RDFBuilder) = vr.toRDF(builder)
}

case class JenaShaclReport(model: Model) extends RDFReport {
    override def toRDF(builder: RDFBuilder) = builder match {
        case rdfJena: RDFAsJenaModel => RDFAsJenaModel.fromModel(model).flatMap(m => rdfJena.merge(m))
        case _ => IO.raiseError(new RuntimeException(s"Builder in JenaShaclReport.toRDF must be RDFAsJenaModel"))
    }
}

object RDFReport {
  def empty: RDFReport = EmptyReport
}
