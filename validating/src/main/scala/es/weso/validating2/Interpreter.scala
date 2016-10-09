package es.weso.validating2

object TypeChecker {
  
  sealed trait DSL[A] 
  
  case class IntConstant[A](x: Int) extends DSL[A]
  case class Var[A](name: String) extends DSL[A]
  
}