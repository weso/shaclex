package es.weso.shex.normalized

import es.weso.shex._

case class Constraint(shape: Option[ShapeExpr],
                      hasExtra: Boolean,
                      card: Cardinality,
                      as: Option[List[Annotation]],
                      tc: TripleConstraint // Reference to original constraint
                     )