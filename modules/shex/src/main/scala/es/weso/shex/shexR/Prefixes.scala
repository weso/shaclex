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
  lazy val sx_IriStem = sx + "IriStem"
  lazy val sx_IriStemRange = sx + "IriStemRange"
  lazy val sx_LanguageStem = sx + "LanguageStem"
  lazy val sx_LanguageStemRange = sx + "LanguageStemRange"
  lazy val sx_LiteralStem = sx + "LiteralStem"
  lazy val sx_LiteralStemRange = sx + "LiteralStemRange"
  lazy val sx_NodeConstraint = sx + "NodeConstraint"
  lazy val sx_OneOf = sx + "OneOf"
  lazy val sx_Schema = sx + "Schema"
  lazy val sx_SemAct = sx + "SemAct"
  lazy val sx_Shape = sx + "Shape"
  lazy val sx_ShapeAnd = sx + "ShapeAnd"
  lazy val sx_ShapeExternal = sx + "ShapeExternal"
  lazy val sx_ShapeNot = sx + "ShapeNot"
  lazy val sx_ShapeOr = sx + "ShapeOr"
  lazy val sx_TripleConstraint = sx + "TripleConstraint"
  lazy val sx_Wildcard = sx + "Wildcard"
  lazy val sx_INF = sx + "INF"

  lazy val sx_annotation = sx + "annotation"
  lazy val sx_bnode = sx + "bnode"
  lazy val sx_code = sx + "code"
  lazy val sx_closed = sx + "closed"
  lazy val sx_datatype = sx + "datatype"
  lazy val sx_extra = sx + "extra"
  lazy val sx_exclusion = sx + "exclusion"
  lazy val sx_expression = sx + "expression"
  lazy val sx_expressions = sx + "expressions"
  lazy val sx_fractiondigits = sx + "fractiondigits"
  lazy val sx_flags = sx + "flags"
  lazy val sx_iri = sx + "iri"
  lazy val sx_inverse = sx + "inverse"
  lazy val sx_length = sx + "length"
  lazy val sx_literal = sx + "literal"
  lazy val sx_min = sx + "min"
  lazy val sx_mininclusive = sx + "mininclusive"
  lazy val sx_minexclusive = sx + "minexclusive"
  lazy val sx_minlength = sx + "minlength"
  lazy val sx_max = sx + "max"
  lazy val sx_maxinclusive = sx + "maxinclusive"
  lazy val sx_maxexclusive = sx + "maxexclusive"
  lazy val sx_maxlength = sx + "maxlength"
  lazy val sx_name = sx + "name"
  lazy val sx_negated = sx + "negated"
  lazy val sx_nodeKind = sx + "nodeKind"
  lazy val sx_nonliteral = sx + "nonliteral"
  lazy val sx_object = sx + "object"
  lazy val sx_pattern = sx + "pattern"
  lazy val sx_predicate = sx + "predicate"
  lazy val sx_semActs = sx + "semActs"
  lazy val sx_startActs = sx + "startActs"
  lazy val sx_start = sx + "start"
  lazy val sx_shapes = sx + "shapes"
  lazy val sx_shapeExprs = sx + "shapeExprs"
  lazy val sx_shapeExpr = sx + "shapeExpr"
  lazy val sx_stem = sx + "stem"
  lazy val sx_stemRange = sx + "stemRange"
  lazy val sx_totaldigits = sx + "totaldigits"
  lazy val sx_valueExpr = sx + "valueExpr"
  lazy val sx_values = sx + "values"
}
