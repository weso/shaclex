package es.weso.shex.spec

import es.weso.rdf.RDFReader
import es.weso.shex.Schema
import es.weso.shex.spec.Check.ShapeTyping

case class Env(schema: Schema, typing: ShapeTyping, rdf: RDFReader)
