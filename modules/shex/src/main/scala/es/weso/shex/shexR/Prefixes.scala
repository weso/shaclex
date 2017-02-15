package es.weso.shex.shexR

import es.weso.rdf.nodes.IRI

/**
 * Common Prefixes for RDF
 */
object PREFIXES {

  // Namespace IRI
  lazy val sx = IRI("http://shex.io/ns/shex#")

  // Classes
  lazy val sx_Annotation = sx + "Annotation"
  lazy val sx_EachOf = sx + "EachOf"
  lazy val sx_NodeConstraint = sx + "NodeConstraint"
  lazy val sx_OneOf = sx + "OneOf"
  lazy val sx_TripleConstraint = sx + "TripleConstraint"
  lazy val sx_Schema = sx + "Schema"
  lazy val sx_SemAct = sx + "SemAct"
  lazy val sx_Shape = sx + "Shape"
  lazy val sx_ShapeAnd = sx + "ShapeAnd"
  lazy val sx_ShapeExternal = sx + "ShapeExternal"
  lazy val sx_ShapeNot = sx + "ShapeNot"
  lazy val sx_ShapeOr = sx + "ShapeOr"

  lazy val sx_annotation = sx + "annotation"
  lazy val sx_bnode = sx + "bnode"
  lazy val sx_code = sx + "code"
  lazy val sx_closed = sx + "closed"
  lazy val sx_datatype = sx + "datatype"
  lazy val sx_extra = sx + "extra"
  lazy val sx_expression = sx + "expression"
  lazy val sx_expressions = sx + "expressions"
  lazy val sx_iri = sx + "iri"
  lazy val sx_inverse = sx + "inverse"
  lazy val sx_literal = sx + "literal"
  lazy val sx_min = sx + "min"
  lazy val sx_max = sx + "max"
  lazy val sx_name = sx + "name"
  lazy val sx_negated = sx + "negated"
  lazy val sx_nodeKind = sx + "nodeKind"
  lazy val sx_nonliteral = sx + "nonliteral"
  lazy val sx_object = sx + "object"
  lazy val sx_predicate = sx + "predicate"
  lazy val sx_semActs = sx + "semActs"
  lazy val sx_startActs = sx + "startActs"
  lazy val sx_start = sx + "start"
  lazy val sx_shapes = sx + "shapes"
  lazy val sx_shapeExprs = sx + "shapeExprs"
  lazy val sx_shapeExpr = sx + "shapeExpr"
  lazy val sx_valueExpr = sx + "valueExpr"


  private val shexMap: Map[String, IRI] =
    Map("sx" -> sx)

}
