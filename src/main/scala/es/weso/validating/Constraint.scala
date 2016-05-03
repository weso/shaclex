package es.weso.validating

import cats.Functor
import cats.implicits._
import ConstraintReason._
import Validated._

sealed abstract class Constraint[Context, A, E >: Throwable] {
  def validate: (A,Context) => Validated[A,ConstraintReason,E]
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
    Constraint.all(cs.map(c => c.validate(x,ctx)))
  }
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
  def validate = (x,ctx) => 
    Constraint.some(cs.map(c => c.validate(x,ctx)))
}

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
  def validate = (x,ctx) => 
    Constraint.oneOf(cs.map(c => c.validate(x,ctx)))
}

case class And[C,A,E >: Throwable](c1: Constraint[C,A,E], c2: Constraint[C,A,E]) extends Constraint[C,A,E] {
  def validate : (A,C) => Validated[A,ConstraintReason,E] = (x,ctx) => {
    lazy val v1 = c1.validate(x,ctx)
    lazy val v2 = c2.validate(x,ctx)
    Constraint.all(Seq(v1,v2))
  }
}

case class Or[C,A,E >: Throwable](c1: Constraint[C,A,E], c2: Constraint[C,A,E]) extends Constraint[C,A,E] {
  def validate : (A,C) => Validated[A,ConstraintReason,E] = (x,ctx) => {
    lazy val v1 = c1.validate(x,ctx)
    lazy val v2 = c2.validate(x,ctx)
    Constraint.or(v1,v2)
  }
}

case class Xor[C,A,E >: Throwable](c1: Constraint[C,A,E], c2: Constraint[C,A,E]) extends Constraint[C,A,E] {
  def validate : (A,C) => Validated[A,ConstraintReason,E] = (x,ctx) => {
    lazy val v1 = c1.validate(x,ctx)
    lazy val v2 = c2.validate(x,ctx)
    Constraint.xor(v1,v2)
  }
}

case class Single[C,A,E >: Throwable](
    validate: (A,C) => Validated[A,ConstraintReason,E]) extends Constraint[C,A,E]

  

object Constraint {

  def all[A,E >: Throwable](vs: Seq[Validated[A,ConstraintReason,E]]): Validated[A,ConstraintReason,E] = {
    def comb(r1: Response[A,ConstraintReason],
             r2: Response[A,ConstraintReason]): Response[A,ConstraintReason] = 
           Response(AllReason(Seq(r1.response,r2.response)))
    val zero : Validated[A,ConstraintReason,E] = Validated.okZero()
    def next(rest: Validated[A,ConstraintReason,E], current: Validated[A,ConstraintReason,E]): Validated[A,ConstraintReason,E] = {
      current.fold(rs1 => 
        rest.fold(rs2 => {
         val rs = rs1.combineWith(rs2,comb)
         Validated.oks(rs) 
        }
        , es => Validated.err(All_SomeNotValid(s"Element not valid: $rest"))
        ),
        es1 => Validated.err(All_SomeNotValid(s"Element not valid: $current")) 
      )
    }
    vs.foldLeft(zero)(next)  
  }
  
  def some[A,E >: Throwable](vs: Seq[Validated[A,ConstraintReason,E]]): Validated[A,ConstraintReason,E] = {
    val zero : Validated[A,ConstraintReason,E] = err(Some_NoOneValid("No one valid"))
    def next(rest: Validated[A,ConstraintReason,E], current: Validated[A,ConstraintReason,E]): Validated[A,ConstraintReason,E] = {
       or(current,rest)
      }
     vs.foldLeft(zero)(next)      
    }
  
  def or[A,E >: Throwable](v1: Validated[A,ConstraintReason,E], v2: Validated[A,ConstraintReason,E]): Validated[A,ConstraintReason,E] = {
     (v1.isOK,v2.isOK) match {
       case (true,true) => {
         val rs1 = v1.reasons.get
         val rs2 = v2.reasons.get
         def comb(
             r1: Response[A,ConstraintReason],
             r2: Response[A,ConstraintReason]): Response[A,ConstraintReason] = 
           Response(SomeReason(Seq(r1.response,r2.response)))
         val rs = rs1.combineWith(rs2,comb)
         Validated.oks(rs)
       }
       
       case (true,false) => {
         val rs1 = v1.reasons.get
         def f(r: Response[A,ConstraintReason]):Response[A,ConstraintReason] = Response(SomeReason(Seq(r.response)))
         Validated.oks(rs1.mapResponse(r => f(r)))
       }
       
       case (false,true) => {
         val rs2 = v2.reasons.get
         def f(r: Response[A,ConstraintReason]):Response[A,ConstraintReason] = Response(SomeReason(Seq(r.response)))
         Validated.oks(rs2.mapResponse(r => f(r)))
       }

       case (false,false) => 
         Validated.err(Some_NoOneValid(s"Both branches are not valid: $v1 and $v2"))
      }
  }

  def oneOf[A,E >: Throwable](vs: Seq[Validated[A,ConstraintReason,E]]): Validated[A,ConstraintReason,E] = {
    val zero : Validated[A,ConstraintReason,E] = err(OneOf_NoOneValid("No one valid"))
    def next(rest: Validated[A,ConstraintReason,E], current: Validated[A,ConstraintReason,E]): Validated[A,ConstraintReason,E] = {
       xor(current,rest)
      }
     vs.foldLeft(zero)(next)      
    }
  
  def xor[A,E >: Throwable](v1: Validated[A,ConstraintReason,E], v2: Validated[A,ConstraintReason,E]): Validated[A,ConstraintReason,E] = {
     (v1.isOK,v2.isOK) match {
       case (true,true) => 
         Validated.err(OneOf_MoreThanOneValid(s"Both branches are valid: $v1 and $v2"))
       
       case (true,false) => {
         val rs1 = v1.reasons.get
         def f(r: Response[A,ConstraintReason]):Response[A,ConstraintReason] = Response(OneOfReason(Seq(r.response)))
         Validated.oks(rs1.mapResponse(r => f(r)))
       }

       case (false,true) => {
         val rs2 = v2.reasons.get
         def f(r: Response[A,ConstraintReason]):Response[A,ConstraintReason] = Response(OneOfReason(Seq(r.response)))
         Validated.oks(rs2.mapResponse(r => f(r)))
       }

       case (false,false) => 
         Validated.err(OneOf_NoOneValid(s"Both branches are not valid: $v1 and $v2"))
      }
  }


}