package es.weso.validating
import cats._
import cats.syntax.all._

sealed abstract class ConstraintReason[A]
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
    
implicit val cReason = new Applicative[ConstraintReason] {
  def map[A,B](fa: ConstraintReason[A])(f:A => B): ConstraintReason[B] = {
    fa match {
      case SomeReason(cs) => SomeReason(cs.map(c => this.map(c)(f)))
      case AllReason(cs) => AllReason(cs.map(c => this.map(c)(f)))
      case OneOfReason(cs) => OneOfReason(cs.map(c => this.map(c)(f)))
      case SingleReason(x,msg) => SingleReason(f(x),msg)
    }
  }
  
  def pure[A](x:A): ConstraintReason[A] = {
    SingleReason(x,"")
  }
  
  def ap[A, B](fn: ConstraintReason[A => B])
        (fa: ConstraintReason[A]): ConstraintReason[B] = {
   fa match {
      case SomeReason(cs) => SomeReason(cs.map(c => this.ap(fn)(c)))
      case AllReason(cs) => AllReason(cs.map(c => this.ap(fn)(c)))
      case OneOfReason(cs) => OneOfReason(cs.map(c => this.ap(fn)(c)))
      case SingleReason(x,msg1) => {
        fn match {
          case SingleReason(f,msg2) => SingleReason(f(x),msg2 ++ msg1)
          case SomeReason(cs) => SomeReason(cs.map(c => ap(c)(fa)))
          case AllReason(cs) => AllReason(cs.map(c => ap(c)(fa)))
          case OneOfReason(cs) => OneOfReason(cs.map(c => ap(c)(fa)))
        }
      }
    } 
  }
  
  // TODO: I supposed that this should not need to be implemented
  // In principle it should be obtained from the others...
  // I took this definition from: https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/Apply.scala
  def product[A, B](fa: ConstraintReason[A],
      fb: ConstraintReason[B]): ConstraintReason[(A, B)] = 
    ap(map(fa)(a => (b: B) => (a, b)))(fb)
/*  def product[A, B](fa: ConstraintReason[A],
      fb: ConstraintReason[B]): ConstraintReason[(A, B)] = {
    fa match {
      case SingleReason(x,msg1) => {
        fb match {
          case SingleReason(y,msg2) => SingleReason((x,y),msg1 + "\n" + msg2)
          case SomeReason(cs) => SomeReason(cs.map(c => product(fa,c)))
          case AllReason(cs) => AllReason(cs.map(c => product(fa,c)))
          case OneOfReason(cs) => OneOfReason(cs.map(c => product(fa,c)))
        }
      }
      case SomeReason(cs) => fb match {
        case SingleReason(y,msg) => SomeReason(cs.map(c => product(c,fb)))
        case SomeReason(cs) => ???
        case AllReason(cs) => ???
        case OneOfReason(cs) => ???
      }
      case AllReason(cs) => fb match {
        case SingleReason(y,msg) => AllReason(cs.map(c => product(c,fb)))
        case SomeReason(cs) => ???
        case AllReason(cs) => ???
        case OneOfReason(cs) => ???
      }
      case OneOfReason(cs) => fb match {
        case SingleReason(y,msg) => OneOfReason(cs.map(c => product(c,fb)))
        case SomeReason(cs) => ???
        case AllReason(cs) => ???
        case OneOfReason(cs) => ???
      }
    }
  } */
  
}

}
