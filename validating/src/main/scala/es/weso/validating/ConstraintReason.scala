package es.weso.validating
import cats._
import cats.syntax.all._

/**
 * An Explanation about why a constraint is satisfied
 * @tparam A type of value that is being validated
 */
sealed abstract class ConstraintReason[A]

/**
 * A Some Constraint is satisfied. 
 * @tparam A type of value that is being validated
 * @param cs set of constraint reasons that are satisfied
 */
case class SomeReason[A](cs: Seq[A]) extends ConstraintReason[A]

/**
 * A All Constraint is satisfied. 
 * @tparam A type of value that is being validated
 * @param cs constraint reasons that are satisfied
 */
case class AllReason[A](cs: Seq[A]) extends ConstraintReason[A]

/**
 * A OneOf Constraint is satisfied. 
 * @tparam A type of value that is being validated
 * @param c constraint reason that is satisfied
 */
case class OneOfReason[A](cs: Seq[A]) extends ConstraintReason[A]

/**
 * A Single Constraint is satisfied. 
 * @tparam A type of value that is being validated
 * @param x value that has been validated
 * @param msg explanation of validation
 */
case class SingleReason[A](msg: String) extends ConstraintReason[A]

case class NotReason[A](c: A) extends ConstraintReason[A]

case class EmptyReason[A]() extends ConstraintReason[A]

object ConstraintReason {


/**
 * Construct a single reason  
 * @tparam A type of value that is being validated
 * @param x value that has been validated
 * @param msg explanation of validation
 */
def singleReason[A](msg:String): ConstraintReason[A] = 
    SingleReason(msg)
    
/**
 * Construct a All constraint reason  
 * @tparam A type of value that is being validated
 * @param cs set of constraint reasons
 */
def allReason[A](cs:A*): ConstraintReason[A] = 
    AllReason(cs)
    
def mkAll[A](c1: ConstraintReason[A], c2: ConstraintReason[A]) = {
  (c1,c2) match {
    case (AllReason(cs1),AllReason(cs2)) => AllReason(cs1 ++ cs2)
    case (AllReason(cs1),EmptyReason()) => AllReason(cs1)
    case (EmptyReason(), AllReason(cs2)) => AllReason(cs2)
    case (_,_) => AllReason(Seq(c1,c2))
  }
}

def mkSome[A](c1: ConstraintReason[A], c2: ConstraintReason[A]) = {
  (c1,c2) match {
    case (SomeReason(cs1),SomeReason(cs2)) => SomeReason(cs1 ++ cs2)
    case (SomeReason(cs1),EmptyReason()) => SomeReason(cs1)
    case (EmptyReason(), SomeReason(cs2)) => SomeReason(cs2)
    case (_,_) => SomeReason(Seq(c1,c2))
  }
}

def mkOneOf[A](c1: ConstraintReason[A], c2: ConstraintReason[A]) = {
  (c1,c2) match {
    case (OneOfReason(cs1),OneOfReason(cs2)) => OneOfReason(cs1 ++ cs2)
    case (OneOfReason(cs1),EmptyReason()) => OneOfReason(cs1)
    case (EmptyReason(), OneOfReason(cs2)) => OneOfReason(cs2)
    case (_,_) => OneOfReason(Seq(c1,c2))
  }
}

/**
 * Construct a Some constraint reason  
 * @tparam A type of value that is being validated
 * @param cs set of constraint reasons
 */
def someReason[A](cs:Seq[A]): ConstraintReason[A] = 
    SomeReason(cs)

/**
 * Implicit value definition so ConstraintReason can be an Applicative instance    
 */
implicit val applicativeConstraintReason = new Functor[ConstraintReason] {
  
  override def map[A,B](fa: ConstraintReason[A])(f:A => B): ConstraintReason[B] = {
    fa match {
      case SomeReason(cs) => SomeReason(cs.map(f(_)))
      case AllReason(cs) => AllReason(cs.map(f(_)))
      case OneOfReason(cs) => OneOfReason(cs.map(f(_)))
      case NotReason(c) => NotReason(f(c))
      case SingleReason(msg) => SingleReason(msg)
      case EmptyReason() => EmptyReason()
    }
  }
  
/*  override def pure[A](x:A): ConstraintReason[A] = {
    SingleReason("")
  } */
  
/*  override def ap[A, B](fn: ConstraintReason[A => B])
        (fa: ConstraintReason[A]): ConstraintReason[B] = {
   fa match {
      case SomeReason(cs) => SomeReason(cs.map(c => fn(c))
      case AllReason(cs) => AllReason(cs.map(ap(fn)(_)))
      case OneOfReason(cs) => OneOfReason(cs.map(ap(fn)(_)))
      case NotReason(c) => NotReason(ap(fn)(c))
      case SingleReason(msg1) => {
        fn match {
          case SingleReason(msg2) => SingleReason(msg2 ++ msg1)
          case SomeReason(cs) => SomeReason(cs.map(ap(_)(fa)))
          case AllReason(cs) => AllReason(cs.map(ap(_)(fa)))
          case OneOfReason(cs) => OneOfReason(cs.map(ap(_)(fa)))
          case NotReason(c) => NotReason(ap(c)(fa))
          case EmptyReason() => EmptyReason()
        }
      }
     case EmptyReason() => EmptyReason()
    } 
  } */
  
  // TODO: I supposed that this should not need to be implemented
  // In principle it should be obtained from the others...
  // I took this definition from: https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/Apply.scala
 /* override def product[A, B](fa: ConstraintReason[A],
      fb: ConstraintReason[B]): ConstraintReason[(A, B)] = 
    ap(map(fa)(a => (b: B) => (a, b)))(fb) */
}

}
