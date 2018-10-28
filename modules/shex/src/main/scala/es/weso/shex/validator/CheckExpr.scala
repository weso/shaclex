package es.weso.shex.validator

import es.weso.shex.{SemAct, ShapeExpr}

trait CheckExpr
case class Pos(se: ShapeExpr, semActs: Option[List[SemAct]]) extends CheckExpr
case class Neg(se: ShapeExpr, semActs: Option[List[SemAct]]) extends CheckExpr
