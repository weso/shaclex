package es.weso.shacl
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import es.weso.rdf._
import es.weso.rdf.nodes._

/*
trait ShapeTypings extends GenericTypings {
  type Evidence = String
  type Key = RDFNode
  type Value = Shape
  type Error = ViolationError

  implicit def showValue = new Show[Shape] {
    def show(shape: Shape): String = {
      shape.id.fold("_?")(iri => iri.str)
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
  
} */