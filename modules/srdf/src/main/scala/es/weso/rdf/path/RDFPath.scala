package es.weso.rdf.path

import es.weso.rdf.nodes.IRI

sealed trait RDFPath {
    def predicate: Option[IRI]
}
case class PredicatePath(iri: IRI) extends RDFPath {
    override def predicate: Option[IRI] = Some(iri)
  }
case class InversePath(iri: IRI) extends RDFPath {
    override def predicate: Option[IRI] = None
}
case class SequencePath(paths: Seq[RDFPath]) extends RDFPath {
    override def predicate: Option[IRI] = None
}
case class AlternativePath(paths: Seq[RDFPath]) extends RDFPath {
    override def predicate: Option[IRI] = None
}
case class ZeroOrMorePath(path: RDFPath) extends RDFPath {
    override def predicate: Option[IRI] = None
}
case class OneOrMorePath(path: RDFPath) extends RDFPath {
    override def predicate: Option[IRI] = None
}
