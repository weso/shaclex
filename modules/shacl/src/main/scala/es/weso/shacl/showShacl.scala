package es.weso.shacl
import cats._
import es.weso.rdf.nodes._
import es.weso.shacl.validator.ViolationError

object showShacl {

  implicit def showShape = new Show[Shape] {
    def show(shape: Shape): String = {
      shape.id.toString // .fold("_?")(iri => iri.str)
    }
  }

  implicit def showError = new Show[ViolationError] {
    def show(ve: ViolationError): String = {
      s"Violation Error(${ve.id}). Node(${ve.focusNode}) ${ve.message.getOrElse("")}"
    }
  }

  implicit def showRDFNode = new Show[RDFNode] {
    def show(n: RDFNode): String = n.toString
  }

}
