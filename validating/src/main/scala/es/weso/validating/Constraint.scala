package es.weso.validating

import cats._
import cats.implicits._
import CheckedSub._
import ConstraintReason._

/**
 * A constraint can be seen as a function from a context and a value that returns a
 * validated value
 *
 * @tparam Context Type of context in which the constraint is evaluated
 * @tparam A Type of values that the constraint validates
 *
 */
@deprecated
sealed abstract class Constraint[Context, A, R] {
  def id: String
  def validate: (A, Context, R) => CheckedVal[A, R]
}

/**
 * Given a set of constraints, creates a constraint that is satisfied when all
 * the constraints are satisfied
 *
 * @tparam C Context
 * @tparam A Type of Values to validate
 *
 */
case class All[C, A: Monoid, R: Monoid](
    cs: Seq[Constraint[C, A, R]]
  ) extends Constraint[C, A, R] {
  override def id = "All"
  override def validate = (x, ctx, r) => {
    Constraint.all(cs.map(c => c.validate(x, ctx, r)))
  }
  override def toString: String = {
    s"$id[${cs.mkString(",")}]"
  }
} 

/**
 * Given a set of constraints, creates a constraint that is satisfied when at least
 * one of the constraints is satisfied
 *
 * @tparam C Context
 * @tparam A Type of Values to validate
 *
 */
case class SomeOf[C,A: Monoid,R:Monoid](
    cs: Seq[Constraint[C,A,R]]
    ) extends Constraint[C,A,R] {
  override def id="SomeOf"
  override def validate = (x,ctx, r) => 
    Constraint.some(cs.map(c => c.validate(x, ctx, r)))
  override def toString: String = {
    s"$id[${cs.mkString(",")}]"
  }
} 

/**
 * Given a set of constraints, creates a constraint that is satisfied when one and only one of
 * the constraints is satisfied
 *
 * @tparam C Context
 * @tparam A Type of Values to validate
 *
 */
case class OneOf[C,A:Monoid,R:Monoid](
    cs: Seq[Constraint[C,A,R]]
    ) extends Constraint[C,A,R] {
  override def id="OneOf"
  override def validate = (x, ctx, r) => 
    Constraint.oneOf(cs.map(c => c.validate(x, ctx, r)))
  override def toString: String = {
    s"$id[${cs.mkString(",")}]"
  }
}

/**
 * Given two constraints, creates a constraint that is satisfied when both are satisfied
 *
 * @tparam C Context
 * @tparam A Type of Values to validate
 *
 */
case class AndConstraint[C,A: Monoid, R: Monoid]
  (c1: Constraint[C,A,R],
   c2: Constraint[C,A,R]) extends Constraint[C,A,R] {
  override def id="And"
  override def validate : (A,C,R) => CheckedVal[A,R] = (x,ctx,r) => {
    lazy val v1 = c1.validate(x,ctx,r)
    lazy val v2 = c2.validate(x,ctx,r)
    Constraint.all(Seq(v1,v2))
  }
  override def toString: String = {
    s"$id[$c1,$c2]"
  }
}

/**
 * Given two constraints, creates a constraint that is satisfied when one or the other are satisfied
 *
 * @tparam C Context
 * @tparam A Type of Values to validate
 *
 */
case class OrConstraint[C, A: Monoid, R: Monoid](
    c1: Constraint[C, A, R],
    c2: Constraint[C, A, R]) extends Constraint[C, A, R] {
  override def id = "Or"
  override def validate: (A, C, R) => CheckedVal[A, R] = (x, ctx, r) => {
    lazy val v1 = c1.validate(x, ctx, r)
    lazy val v2 = c2.validate(x, ctx, r)
    Constraint.some2(v1, v2)
  }
  override def toString: String = {
    s"$id[$c1,$c2]"
  }
}

/**
 * Given two constraints, creates a constraint that is satisfied when one or the other, bot not both, are satisfied
 *
 * @tparam C Context
 * @tparam A Type of Values to validate
 *
 */
case class Xor[C, A: Monoid, R: Monoid](
    c1: Constraint[C, A, R],
    c2: Constraint[C, A, R]) extends Constraint[C, A, R] {
  override def id = "Xor"
  override def validate = (x, ctx, r) => {
    lazy val v1 = c1.validate(x, ctx, r)
    lazy val v2 = c2.validate(x, ctx, r)
    Constraint.xor(v1, v2)
  }
  override def toString: String = {
    s"$id[$c1,$c2]"
  }
}

/**
 * Given two constraints, creates a constraint that is satisfied when one or the other, bot not both, are satisfied
 *
 * @tparam C Context
 * @tparam A Type of Values to validate
 *
 */
case class Not[C, A:Monoid, R: Monoid](
    c: Constraint[C, A, R]
    ) extends Constraint[C, A, R] {
  override def id = "Xor"
  override def validate = (x, ctx, r) => {
    lazy val v = c.validate(x, ctx, r)
    Constraint.not(v)
  }
  override def toString: String = {
    s"$id[$c]"
  }
}

/**
 * A single constraint which is not composed of other constraints
 *
 * @tparam C Context
 * @tparam A Type of Values to validate
 *
 */
case class Single[C, A, R](
    validate: (A, C, R) => CheckedVal[A, R]) extends Constraint[C, A, R] {
  override def id = "Single"
  override def toString: String = "<single>"
}

object Constraint {

//  type Checked_[A, R] = Checked[ConstraintReason[A], R, ConstraintError[A]]

  /**
   * Creates a single constraint
   *
   * @tparam C Context
   * @tparam A Type of Values to validate
   * @param fn Validation function
   */
  def single[C, A, R](fn: (A, C, R) => CheckedVal[A, R]): Constraint[C, A, R] =
    Single(fn)

  /**
   * Creates a validated value from a list of validated values that is satisfied when all are satisfied
   *
   * @tparam A Type of Values to validate
   * @param vs Sequence of validated values
   */
  def all[A, R: Monoid](
      vs: Seq[CheckedVal[A, R]], 
      shortCircuit: Boolean = true): CheckedVal[A, R] = {
    val zero: CheckedVal[A, R] = CheckedVal(ok(None,(EmptyReason(), Monoid[R].empty)))
    def next(rest: CheckedVal[A, R], current: CheckedVal[A, R]): CheckedVal[A, R] = {
      all2(current,rest,shortCircuit)
    }
    vs.foldLeft(zero)(next)
  }
  
  implicit def ValidationStateSemiGroup[A,R:Semigroup]: Semigroup[(ConstraintReason[A],R)] = 
    new Semigroup[(ConstraintReason[A],R)] {
    override def combine(
        v1: (ConstraintReason[A],R),
        v2: (ConstraintReason[A],R)): (ConstraintReason[A],R) = ???
  }

  def all2[A, R: Monoid](
      v1: CheckedVal[A, R],
      v2: => CheckedVal[A, R],
      shortCircuit: Boolean = true): CheckedVal[A, R] = {
    v1.fold(
        (x1,rs1) => v2.fold(
            (x2,rs2) => CheckedVal(oks(None, mergeSeq(rs1,rs2))),
            (x2,es2) => CheckedVal(err(None, All_SomeNotValid(s"Element not valid: $v2", es2)))
        ),
        (x1,es1) => if (shortCircuit) {
          CheckedVal(err(None, All_SomeNotValid(s"Element not valid: $v1", es1)))
        } else {
          v2.fold(
              (x2,rs2) => CheckedVal(err(None, All_SomeNotValid(s"Element not valid: $v1", es1))),
              (x2,es2) => CheckedVal(err(None, All_SomeNotValid(s"Elements not valid: $v1, $v2", es1 ++ es2)))
          )
        }
            
    )
  }
  

  /**
   * Creates a checked value from a list of validated values that is satisfied when at least one is satisfied
   *
   * @tparam A Type of Values to validate
   * @param vs Sequence of validated values
   */
  def some[A, R: Monoid](
    vs: Seq[CheckedVal[A, R]],
    shortCircuit: Boolean = true
    ): CheckedVal[A, R] = {
    val zero : CheckedVal[A,R] = CheckedVal(err(None, Some_NoOneValid("No one valid")))
    def next(rest: CheckedVal[A, R], current: CheckedVal[A, R]): CheckedVal[A, R] = {
      some2(current, rest, shortCircuit)
    }
    vs.foldLeft(zero)(next)
  }

  /**
   * Creates a validated value from two validated values that is satisfied when at least one of them is satisfied
   *
   * @tparam A Type of Values to validate
   * @param v1 First validated value
   * @param v2 Second validated value
   */
  def some2[A, R: Monoid](
    v1: CheckedVal[A, R],
    v2: => CheckedVal[A, R],
    shortCircuit: Boolean = true): CheckedVal[A, R] = {
    if (v1.isOK) {
      if (shortCircuit)
        v1
      else {
        if (v2.isOK) {
          CheckedVal(oks(None, mergeSeq(v1.checked.responses, v2.checked.responses)))
        } else {
          CheckedVal(oks(None,v1.checked.responses))
        }
      }
    } else {
      if (v2.isOK) {
        CheckedVal(oks(None, v1.checked.responses))
      } else {
        CheckedVal(err(None,
          Some_NoOneValid(s"Both branches are not valid: $v1 and $v2")))
      }
    }
  }

  /**
   * Checks that only one of a list of values is ok
   *
   * @tparam A Type of Values to validate
   * @param vs Sequence of validated values
   */
  def oneOf[A, R: Monoid](
    vs: Seq[CheckedVal[A, R]]): CheckedVal[A, R] = {
    val zero: CheckedVal[A, R] = {
      CheckedVal(err(None, OneOf_NoOneValid("No one valid")))
    }
    def next(rest: CheckedVal[A, R],
             current: CheckedVal[A, R]): CheckedVal[A, R] = {
      xor(current, rest)
    }
    vs.foldLeft(zero)(next)
  }

  /**
   * Creates a validated value from two validated values that is satisfied when only one is satisfied
   *
   * @tparam A Type of Values to validate
   * @param v1 First validated value
   * @param v2 Second validated value
   */
  def xor[A, R: Monoid](
    v1: CheckedVal[A, R],
    v2: CheckedVal[A, R]): CheckedVal[A, R] = {
    (v1.isOK, v2.isOK) match {
      case (true, true) => {
        val e: ConstraintError[A] = OneOf_MoreThanOneValid(s"Both branches are valid: $v1 and $v2", Seq(v1, v2))
        CheckedVal(err(None, e))
      }
      case (true, false) => {
        CheckedVal(oks(None, v1.checked.responses.map(r => ???)))
      }
      case (false, true) => {
        CheckedVal(oks(None, v2.checked.responses.map(r => ???)))
      }
      case (false, false) => {
        val e: ConstraintError[A] = OneOf_NoOneValid(s"Both branches are not valid: $v1 and $v2")
        CheckedVal(err(None, e))
      }
    }
  } 
  
  def not[A, R: Monoid](
    v: CheckedVal[A, R]): CheckedVal[A, R] = {
    if (v.isOK) { 
      val e: ConstraintError[A] =
          Not_ButValid(s"Not applied to a valid constraint: $v", v)
      CheckedVal(err(v.value, e))
    } else  
      CheckedVal(ok(v.value, (NotReason(v), Monoid[R].empty)))
  }
  /**
   * Checks that a value satisfies all of the conditions
   *
   * @tparam A type of value
   * @param x value to check
   * @param conds a sequence of conditions
   * @return the value if all the conditions are ok, otherwise, the accumulated errors
   */
  def checkAll[A: Monoid, R: Monoid](
    x: A,
    conds: Seq[A => CheckedVal[A,R]],
    shortCircuit: Boolean = true): CheckedVal[A, R] = {
    val cs = conds.map(c => c(x))
    Constraint.all(cs,shortCircuit)
  }

  /**
   * Checks if a value satisfies at least one of the conditions
   *
   * <p> This combinator returns a checker for the value but doesn't keep track
   * about which of the conditions was satisfied
   *
   * @tparam A type of value
   * @param x value to check
   * @param conds sequence of conditions
   * @return a checker for value x that is ok if at least one of the conditions is satisfied
   */
  def checkSome[A, R: Monoid](
      x: A,
      conds: Seq[A => CheckedVal[A,R]],
      shortCircuit: Boolean = true): CheckedVal[A, R] = {
    val cs = conds.map(c => c(x))
    Constraint.some(cs, shortCircuit)
  }

  def checkOneOf[A, R: Monoid](
      x: A,
      conds: Seq[A => CheckedVal[A,R]]): CheckedVal[A, R] = {
    val cs = conds.map(c => c(x))
    Constraint.oneOf(cs)
  }
  
  def unsupported[C, 
    A: Monoid, 
    R: Monoid](msg: String): Constraint[C, A, R] = {
    val e: CheckedVal[A, R] =
      errString(Monoid[A].empty, msg)
    def fn(x: A, ctx: C, r: R): CheckedVal[A,R] = e
    ??? // single(fn)
  }

/*  def err[A, R](x: ConstraintReason[A], e: ConstraintError[A]): Checked[ConstraintReason[A], R, ConstraintError[A]] = {
    Checked.err(x, e)
  } */

  /**
   * Creates a checked value which contains an error String
   *
   * @tparam A type of values (ignored)
   * @param msg error
   */
  def errString[A, R](
    x: A,
    msg: String): CheckedVal[A,R] = {
    CheckedVal(err(Some(x), MsgError(msg)))
  }
  
  def okEmpty[A, R: Monoid](): CheckedVal[A,R] = 
    CheckedVal(ok(None,(EmptyReason(),Monoid[R].empty)))

  def okSingle[A, R](
    x: A, r: R, msg: String): CheckedVal[A, R] = {
    CheckedVal(ok(Some(x),(singleReason(msg), r)))
  }

  def okSingle[A, R](
    x: A, 
    rs: Seq[CheckedInfo[A,R]], 
    msg: String): CheckedVal[A, R] = {
    val reason : ConstraintReason_[A,R] = singleReason(msg)
    ??? // CheckedVal(oks(Some(x),rs.map(r => (reason, r))))
  }
  
  /**
   * Converts a boolean condition into a single constraint
   *
   * @param c boolean condition
   * @param msg msg that identifies the boolean condition
   * @tparam A type of values
   * @return a function that takes a value and returns a checked value
   */
  def cond[A, R](
    c: A => Boolean,
    r: R, msg: String): A => CheckedVal[A,R] = { x =>
    if (c(x)) okSingle(x, r, s"$msg passed")
    else errString(x, s"$msg not passed")
  }

  def mapErrors[A,B,C,R]
    (checked: Checked[ConstraintReason[A],R,ConstraintError[B]],
     f: B => C): Checked[ConstraintReason[A],R,ConstraintError[C]] = {
    checked.mapErrors(ce => ce.map(f))
  }
  

  def combineAll[A: Monoid, B: Monoid, R: Monoid](
      values: Seq[A], 
      current: R,
      comb: (A,R) => CheckedVal[B,R]): CheckedVal[B,R] = {
    val cs = values.map(v => comb(v,current))
    all(cs,true)
  }
  
  def merge[A,R:Monoid](cv1: CheckedVal[A,R], cv2: => CheckedVal[A,R]): CheckedVal[A,R] = {
    all2(cv1,cv2)
  }
}
