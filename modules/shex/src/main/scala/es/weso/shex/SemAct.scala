package es.weso.shex

import es.weso.rdf.nodes.IRI

case class SemAct(name: IRI, code: Option[String]) {
  def relativize(base: IRI): SemAct =
    SemAct(name.relativizeIRI(base), code)
}
