package es.weso.validating
import cats._
import cats.syntax.all._

sealed abstract class ConstraintReason[A]
case class SeqReason[A](c1: ConstraintReason[A], c2: ConstraintReason[A]) extends ConstraintReason[A]
case class SomeReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]
case class AllReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]
case class OneOfReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]
case class SingleReason[A](x: A, msg: String) extends ConstraintReason[A]

object ConstraintReason {


def singleReason[A](x:A,msg:String): ConstraintReason[A] = 
    SingleReason(x,msg)
    
def allReason[A](cs:Seq[ConstraintReason[A]]): ConstraintReason[A] = 
    AllReason(cs)

def someReason[A](cs:Seq[ConstraintReason[A]]): ConstraintReason[A] = 
    SomeReason(cs)

implicit val cReasonFunctor = new Functor[ConstraintReason] {
  def map[A,B](fa: ConstraintReason[A])(f:A => B): ConstraintReason[B] = {
    fa match {
      case SomeReason(cs) => SomeReason(cs.map(c => this.map(c)(f)))
      case AllReason(cs) => AllReason(cs.map(c => this.map(c)(f)))
      case OneOfReason(cs) => OneOfReason(cs.map(c => this.map(c)(f)))
      case SingleReason(x,msg) => SingleReason(f(x),msg)
      case SeqReason(c1,c2) => SeqReason(this.map(c1)(f),this.map(c2)(f))
    }
  }
}
    
/*implicit val cReasonPure= new Pure[ConstraintReason] {
  def pure[A](x: A): ConstraintReason[A] = SingleReason(x,"single") 
    
} */

/*implicit val cReasonApply = new Apply[ConstraintReason] {
  def apply[A,B](ra: ConstraintReason[A])(f: A => B): ConstraintReason[B] = ??? 
    
}

implicit val cReasonCartesian = new Cartesian[ConstraintReason] {
  def product[A,B](ra: ConstraintReason[A], rb: ConstraintReason[B]): ConstraintReason[(A,B)] = ??? 
} */

}
