package es.weso.shacl
import cats._, data._
import implicits._
import es.weso.rdf.nodes._

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

  implicit def showEvidence = new Show[String] {
    def show(e: String): String = e
  }

  implicit def showRDFNode = new Show[RDFNode] {
    def show(n: RDFNode): String = n.toString
  }

}
