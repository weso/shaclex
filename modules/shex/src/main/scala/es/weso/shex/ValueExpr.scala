package es.weso.shex

object values {

  abstract sealed trait ValueExpr

  case class Equals(e1: ValueExpr, e2: ValueExpr) extends ValueExpr

  case class Add(e1: ValueExpr, e2: ValueExpr) extends ValueExpr

  case class VariableRef(label: ShapeLabel) extends ValueExpr

}

