package es.weso.shex

abstract sealed trait ValueExpr

case class Eq(e1: ValueExpr, e2:ValueExpr) extends ValueExpr
case class Add(e1: ValueExpr, e2: ValueExpr) extends ValueExpr
case class VariableRef(label: ShapeLabel) extends ValueExpr


