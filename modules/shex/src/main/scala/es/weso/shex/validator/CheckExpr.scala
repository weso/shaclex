package es.weso.shex.validator

import es.weso.shex.ShapeExpr

trait CheckExpr
case class Pos(se: ShapeExpr) extends CheckExpr
case class Neg(se: ShapeExpr) extends CheckExpr
