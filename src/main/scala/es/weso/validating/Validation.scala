package es.weso.validating

import org.scalactic._

trait Validation {

  type Constraint[A,Explain, Error, Context] = Context => A => Validated[A,Explain,Error]

  sealed class Validated[+A,+Explanation,+Error] 
  
  case class OK[A, Explanation](value: A, explanation: Explanation) 
    extends Validated[A,Explanation,Nothing]
  
  case class Err[Error](errors: Seq[Error]) 
    extends Validated[Nothing,Nothing,Error]
  
}

object Validation {
  
  def err[A,Explanation,Error](e: Error): Validated[A,Explanation,Error] = Err(Seq(e))
  
  def ok[A,Explanation,Error](x:A, explanation: Explanation): Validated[A,Explanation,Error] = OK(x,explanation)
}
