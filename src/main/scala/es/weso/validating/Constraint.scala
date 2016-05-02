package es.weso.validating

import cats.Functor
import cats.implicits._
import ConstraintReason._

sealed abstract class Constraint[Context, A, E >: Throwable] {
  def validate: (A,Context) => Validated[A,ConstraintReason,E]
}

  /**
   * Given a set of constraints, creates a constraint that is satisfied when at least 
   * one of the constraints is satisfied
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * @tparam R Type of reasons that explain the validation
   * @tparam E Type of errors
   * 
   */
case class SomeOf[C,A,E >: Throwable](cs: Seq[Constraint[C,A,E]]) extends Constraint[C,A,E] {
  def validate = (x,ctx) => {
    val zero : Validated[A,ConstraintReason,E] = Validated.err(NoneValid)
    def op(rest: Validated[A,ConstraintReason,E], c: Constraint[C,A,E]): Validated[A,ConstraintReason,E] = {
      val current = c.validate(x,ctx)
      rest.combineSome(current)
    }
    cs.foldLeft(zero)(op)
  } 
}

 /**
   * Given a set of constraints, creates a constraint that is satisfied when at least 
   * one of the constraints is satisfied
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * @tparam E Type of errors
   * 
   */
case class All[C,A,E >: Throwable](cs: Seq[Constraint[C,A,E]]) extends Constraint[C,A,E] {
  def validate = (x,ctx) => {
    val zero : Validated[A,ConstraintReason,E] = Validated.okZero()
    def op(rest: Validated[A,ConstraintReason,E], c: Constraint[C,A,E]): Validated[A,ConstraintReason,E] = {
      val current = c.validate(x,ctx)
      rest.combineAll(current)
    }
    cs.foldLeft(zero)(op)
  }
}

case class Single[C,A,E >: Throwable](
    validate: (A,C) => Validated[A,ConstraintReason,E]) extends Constraint[C,A,E]

  /**
   * Given a set of constraints, creates a constraint that is satisfied when one of
   * the constraints is satisfied
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * @tparam R Type of reasons that explain the validation
   * @tparam E Type of errors
   * 
   */
case class OneOf[C,A,E >: Throwable](cs: Seq[Constraint[C,A,E]]) extends Constraint[C,A,E] {
  def validate = (x, ctx) => {
    val zero : Validated[A,ConstraintReason,E] = Validated.err(NoneValid)
    def op(rest: Validated[A,ConstraintReason,E], c: Constraint[C,A,E]): Validated[A,ConstraintReason,E] = {
      val current = c.validate(x,ctx)
      rest.combineOne(current)
    }
    cs.foldLeft(zero)(op)
  }
}
  
object Constraint {

}

