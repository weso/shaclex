package es.weso.shex
import es.weso.rdf.nodes.IRI


sealed trait Path
case class Direct(p: IRI) extends Path
case class Inverse(p: IRI) extends Path
// TODO: Handle sequence and alternative paths

