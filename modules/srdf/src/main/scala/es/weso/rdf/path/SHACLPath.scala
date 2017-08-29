package es.weso.rdf.path

import es.weso.rdf.nodes.IRI

sealed trait SHACLPath {
  def predicate: Option[IRI]
}
case class PredicatePath(iri: IRI) extends SHACLPath {
  override def predicate: Option[IRI] = Some(iri)
}
case class InversePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class SequencePath(paths: Seq[SHACLPath]) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class AlternativePath(paths: Seq[SHACLPath]) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class ZeroOrMorePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class OneOrMorePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class ZeroOrOnePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
