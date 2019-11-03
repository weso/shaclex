package es.weso.shextest.manifest

import es.weso.rdf.nodes._

sealed trait Result {

    def asBoolean: Option[Boolean] = {
      this match {
        case BooleanResult(b) => Some(b)
        case _ => None
      }
    }
  
    val isValid: Boolean
  
    def resultShapeMap: Option[IRI] = this match {
      case ResultShapeMapIRI(iri) => Some(iri)
      case _ => None
    }
  }
  
  final case class ResultShapeMapIRI(iri: IRI) extends Result {
    override val isValid = false
  }
  
  case class ValidPair(
    node: RDFNode,
    shape: RDFNode)
  
  final case class BooleanResult(value: Boolean) extends Result {
    override val isValid = value
  }
  
  final case class IRIResult(
    value: IRI) extends Result {
    override val isValid = false
  }
  
  final case object EmptyResult
    extends Result {
    override val isValid = true
  }
  
  
  case class Status(iri: IRI)