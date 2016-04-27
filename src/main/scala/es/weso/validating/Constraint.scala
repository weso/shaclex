package es.weso.validating

sealed abstract class ConstraintReason[A]
case class OrReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]
case class AllReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]
case class OneOfReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]

sealed abstract class Constraint[Context,A,R, E >: Throwable] {
  def validate: (A,Context) => Validated[A,R,E]
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
case class SomeOf[C,A,R,E >: Throwable](cs: Seq[Constraint[C,A,R,E]]) extends Constraint[C,A,R,E] {
  def validate = (x,ctx) => {
    val zero : Validated[A,R,E] = Validated.err(NoneValid)
    def op(rest: Validated[A,R,E], c: Constraint[C,A,R,E]): Validated[A,R,E] = {
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
case class All[C,A,R,E >: Throwable](cs: Seq[Constraint[C,A,R,E]]) extends Constraint[C,A,R,E] {
  def validate = (x,ctx) => {
    val zero : Validated[A,R,E] = Validated.okZero()
    def op(rest: Validated[A,R,E], c: Constraint[C,A,R,E]): Validated[A,R,E] = {
      val current = c.validate(x,ctx)
      rest.combineAll(current)
    }
    cs.foldLeft(zero)(op)
  }
}

case class Single[C,A,R,E >: Throwable](
    validate: (A,C) => Validated[A,R,E]) extends Constraint[C,A,R,E]

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
case class OneOf[C,A,R,E >: Throwable](cs: Seq[Constraint[C,A,R,E]]) extends Constraint[C,A,R,E] {
  def validate = (x, ctx) => {
    val zero : Validated[A,R,E] = Validated.err(NoneValid)
    def op(rest: Validated[A,R,E], c: Constraint[C,A,R,E]): Validated[A,R,E] = {
      val current = c.validate(x,ctx)
      rest.combineOne(current)
    }
    cs.foldLeft(zero)(op)
  }
}
  
object Constraint {

}