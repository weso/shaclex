package es.weso.validating

import cats.Functor
import cats.implicits._

/**
  * Represents Constraint errors
  */
abstract class ConstraintError[A] {
  def msg: String
}

/**
  * Constraint violation when there was an All constraint but some are not valid
  */
case class All_SomeNotValid[A](msg: String, notValids: Seq[ConstraintError[A]]) extends ConstraintError[A]

case class Not_ButValid[A,R](msg: String, v: CheckedVal[A,R]) extends ConstraintError[A]

/**
  * Constraint violation when there was a Some constraint but no one is valid
  */
case class Some_NoOneValid[A](msg: String) extends ConstraintError[A]

/**
  * Constraint violation when there was a OneOf constraint and no one is valid
  */
case class OneOf_NoOneValid[A](msg: String) extends ConstraintError[A]

/**
  * Constraint violation when there was a OneOf constraint and more than one is valid
  */
case class OneOf_MoreThanOneValid[A,R]
(msg: String,
 valids: Seq[CheckedVal[A, R]]) extends ConstraintError[A]

/**
  * Message based error
  */
case class MsgError[A](msg: String) extends ConstraintError[A]

object ConstraintError {

  implicit val cErrorFunctor: Functor[ConstraintError] = new Functor[ConstraintError] {
    override def map[A, B](ce: ConstraintError[A])(f: A => B): ConstraintError[B] = {
      ce match {
        case All_SomeNotValid(msg, cs) => {
          val fn: ConstraintError[A] => ConstraintError[B] = _.map(f)
          val csb: Seq[ConstraintError[B]] = cs.map(fn)
          All_SomeNotValid(msg, csb)
        }
        case Some_NoOneValid(msg) => Some_NoOneValid(msg)
        case OneOf_NoOneValid(msg) => OneOf_NoOneValid(msg)
/*        case Not_ButValid(msg,c) => Not_ButValid(msg,c)
        case OneOf_MoreThanOneValid(msg, vs) => {
          val vsb: Seq[Checked[ConstraintReason[B],R, ConstraintError[B]]] = {
            vs.map(c => {
              val vb: Checked[ConstraintReason[B], R, ConstraintError[A]] = c.mapValue(f)
              val vbes: Checked[ConstraintReason[B], R, ConstraintError[B]] = vb.mapErrors(_.map(f))
              vbes
            }) 
          } 
          OneOf_MoreThanOneValid(msg, vsb)
        } */
      case MsgError(msg) => MsgError(msg)
      case _ => throw new RuntimeException(s"map ConstraintError: Undefined map of constraint error type: $ce")
    }
   }
  }
 
}
