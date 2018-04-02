package es.weso.shacl
import cats._
import es.weso.rdf.nodes._
import es.weso.shacl.report.ValidationResult

object showShacl {

  implicit def showShape = new Show[Shape] {
    def show(shape: Shape): String = {
      shape.id.toString // .fold("_?")(iri => iri.str)
    }
  }

  implicit def showError = new Show[ValidationResult] {
    def show(ve: ValidationResult): String = {
      s"Violation Error(${ve.sourceConstraintComponent}). Node(${ve.focusNode}) ${ve.message.mkString(",")}"
    }
  }

  implicit def showRDFNode = new Show[RDFNode] {
    def show(n: RDFNode): String = n.toString
  }

}
