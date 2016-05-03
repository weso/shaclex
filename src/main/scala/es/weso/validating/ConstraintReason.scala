package es.weso.validating
import cats.Functor

sealed abstract class ConstraintReason[A]
case class SomeReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]
case class AllReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]
case class OneOfReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]
case class SingleReason[A](x: A, msg: String) extends ConstraintReason[A]

object ConstraintReason {

implicit val cReasonFunctor = new Functor[ConstraintReason] {
  
  def map[A,B](fa: ConstraintReason[A])(f:A => B): ConstraintReason[B] = {
    fa match {
      case SomeReason(cs) => SomeReason(cs.map(c => this.map(c)(f)))
      case AllReason(cs) => AllReason(cs.map(c => this.map(c)(f)))
      case OneOfReason(cs) => OneOfReason(cs.map(c => this.map(c)(f)))
      case SingleReason(x,msg) => SingleReason(f(x),msg)
    }
  }
}

}
