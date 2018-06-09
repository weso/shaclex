package es.weso.shex

import es.weso.rdf.nodes.RDFNode

object values {

  abstract sealed trait BinOp {
    def repr: String
  }

  case object Equals extends BinOp {
    override def repr = "="
  }
  case object NotEquals extends BinOp {
    override def repr = "!="
  }
  case object Add extends BinOp {
    override def repr = "+"
  }
  case object Mul extends BinOp {
    override def repr = "*"
  }
  case object Minus extends BinOp {
    override def repr = "-"
  }
  case object Div extends BinOp {
    override def repr = "/"
  }
  case object LT extends BinOp {
    override def repr = "<"
  }
  case object GT extends BinOp {
    override def repr = ">"
  }
  case object LE extends BinOp {
    override def repr = "<="
  }
  case object GE extends BinOp {
    override def repr = ">="
  }

  abstract sealed trait ValueExpr

  case class BinExpr(e1: ValueExpr, op: BinOp, e2: ValueExpr) extends ValueExpr
  case class Var(name: VarName) extends ValueExpr
  case class Const(node: RDFNode) extends ValueExpr


}

