package es.weso
import cats.Monoid

/**
 * Defines validating elements
 * [[Checked]] 
 * [[Constraint]] 
 * [[CheckedVal]] 
 */
package object validating {
  
  /**
   * Checked values that have been checked
   * They can contain a sequence of responses or a sequence of constraint errors
   */
  type ConstraintReason_[A,R] = ConstraintReason[CheckedVal[A,R]]
  type CheckedInfo[A,R] = (ConstraintReason_[A,R],R)
  
  case class CheckedVal[A,R](
      checked: Checked[A, CheckedInfo[A,R], ConstraintError[A]]) {
    def value: Option[A] = checked.value
    def isOK: Boolean = checked.isOK
    def fold[V](oks: (Option[A], 
        Seq[CheckedInfo[A,R]]) => V,
        errs: (Option[A], Seq[ConstraintError[A]]) => V): V = 
          checked.fold(oks,errs)
          
    def responses = checked.responses
    def isNotOK = checked.isNotOK
    def errors = checked.errors
  }

  
}