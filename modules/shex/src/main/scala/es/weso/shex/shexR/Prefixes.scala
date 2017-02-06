package es.weso.shex.shexR

import es.weso.rdf.nodes.IRI

/**
 * Common Prefixes for RDF
 */
object PREFIXES {

  lazy val sx = IRI("http://shex.io/ns/shex#")
  lazy val sx_Schema = sx + "Schema"
  lazy val sx_ShapeOr = sx + "ShapeOr"
  lazy val sx_ShapeAnd = sx + "ShapeAnd"
  lazy val sx_ShapeNot = sx + "ShapeNot"
  lazy val sx_Shape = sx + "Shape"
  lazy val sx_NodeConstraint = sx + "NodeConstraint"
  lazy val sx_ShapeExternal = sx + "ShapeExternal"
  lazy val sx_iri = sx + "iri"
  lazy val sx_bnode = sx + "bnode"
  lazy val sx_literal = sx + "literal"
  lazy val sx_nonliteral = sx + "nonliteral"
  lazy val sx_closed = sx + "closed"
  lazy val sx_extra = sx + "extra"
  lazy val sx_expression = sx + "expression"
  lazy val sx_semActs = sx + "semActs"

  lazy val sx_nodeKind = sx + "nodeKind"
  lazy val sx_startActs = sx + "startActs"
  lazy val sx_start = sx + "start"
  lazy val sx_shapes = sx + "shapes"
  lazy val sx_shapeExprs = sx + "shapeExprs"
  lazy val sx_shapeExpr = sx + "shapeExpr"


  private val shexMap: Map[String, IRI] =
    Map("sx" -> sx)

}
