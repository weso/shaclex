package es.weso.validating
import cats._

/**
 * @tparam A type of values that are checked
 * @tparam R type of responses
 */
trait CheckedV[A,R] {
  
  /**
   * Simple OK value
   */
  def okSingle[R](x: A, r: R, msg: String = ""): CheckedV[A,R]
  
  /**
   * Simple OK value with several responses
   */
  def okSingleND(x: A, rs: Seq[R], msg: String = ""): CheckedV[A,R]
  
  /**
   * Error 
   */
  def errString[A:Monoid,R:Monoid](msg: String): CheckedV[A,R]
  
  /**
   * Error 
   */
  def errValue[A:Monoid,R:Monoid](x:A, msg: String): CheckedV[A,R]
  
  def cond[A, R](c: A => Boolean, r: R, msg: String): A => CheckedV[A,R]
    
  def checkSome[A, R: Monoid](
      x: A,
      conds: Seq[A => CheckedV[A,R]],
      shortCircuit: Boolean = true): CheckedV[A, R]
  
  def checkAll[A, R: Monoid](
      x: A,
      conds: Seq[A => CheckedV[A,R]],
      shortCircuit: Boolean = true): CheckedV[A, R]
  
  def not[A, R: Monoid](v: CheckedV[A, R]): CheckedV[A, R] 
  def and[A, R: Monoid](v1: CheckedV[A, R],v2: CheckedV[A, R]): CheckedV[A, R]
  def or[A, R: Monoid](v1: CheckedV[A, R],v2: CheckedV[A, R]): CheckedV[A, R]
  def xor[A, R: Monoid](v1: CheckedV[A, R],v2: CheckedV[A, R]): CheckedV[A, R]
  def all[A, R: Monoid](vs: Seq[CheckedV[A, R]], shortcircuit: Boolean = true): CheckedV[A, R]
  def some[A, R: Monoid](vs: Seq[CheckedV[A, R]], shortcircuit: Boolean = true): CheckedV[A, R]
  def oneOf[A, R: Monoid](vs: Seq[CheckedV[A, R]]): CheckedV[A, R]
  
}

