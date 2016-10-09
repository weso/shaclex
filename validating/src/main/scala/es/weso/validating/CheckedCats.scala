package es.weso.validating
import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._

/**
 * Checked implementation using Cats
 */
object CheckedCats extends CheckedOps {

  case class CheckedCats[A,R,E](
      value: Option[A], 
      result: ValidatedNel[E, Seq[R]])  extends Checked[A,R,E] {

    override def isOK = result.isValid

    override def responses : Seq[R] = 
      result.fold(_ => Seq(), rs => rs)
      
    override def errors = 
      result.fold(_.toList , _ => Seq())
    

    override def mapValue[B](f: A => B): Checked[B, R, E] = 
      CheckedCats(value.map(f), result)

    override def fold[V](oks: (Option[A], Seq[R]) => V, errs: (Option[A], Seq[E]) => V): V = 
      ???

    override def mapErrors[E1](f: E => E1): Checked[A, R, E1] =
      ??? // CheckedCats(value,
      
    override def addErrors(es: Seq[E]): Checked[A, R, E] = 
      ???

  }

 def err[A, R, E](v: Option[A], e: E): Checked[A, R, E] =
    CheckedCats(v, Invalid(NonEmptyList.fromList(List(e)).get))

 def errs[A, R, E](v: Option[A], es: Seq[E]): Checked[A, R, E] = {
   require(!es.isEmpty)
   CheckedCats(v, Invalid(NonEmptyList.fromList(es.toList).get))
 }
    
 def okZeroSeq[A, R, E](): Checked[Seq[A], R, E] =
    ??? // Valid(Seq(), Seq())
    
 def ok[A, R, E](v: Option[A], r: R): Checked[A, R, E] =
    CheckedCats(v, Valid(Seq(r)))
    
 def oks[A, R, E](v: Option[A], rs: Seq[R]): Checked[A, R, E] =
    CheckedCats(v, Valid(rs))

/*  def checkAll[A, R:Semigroup, E](
      vs: Seq[Checked[A, R, E]]
   ): Checked[Seq[A], R, E] = {
    val zero: Checked[Seq[A], R, E] = okZeroSeq()
    def next(
      v: Checked[A, R, E],
      rest: Checked[Seq[A], R, E]
    ): Checked[Seq[A], R, E] = {
      merge(v,rest)
    }
    vs.foldRight(zero)(next)
  }

  def mergeSeq[R: Semigroup](
      rs1: Seq[R], 
      rs2: Seq[R]): Seq[R] = {
    if (rs1.isEmpty) rs2
    else if (rs2.isEmpty) rs1 
    else
    for (r1 <- rs1; r2 <- rs2)
        yield Semigroup[R].combine(r1,r2)
  }

  def merge[A, R: Semigroup, E](
    first: Checked[A, R, E],
    others: => Checked[Seq[A], R, E]
  ): Checked[Seq[A], R, E] =
    first match {
      case Valid(v, rs1) => others match {
        case Valid(vs2, rs2) => Valid(v +: vs2, mergeSeq(rs1,rs2))
        case Invalid(vs2, es) => Invalid(v +: vs2, es)
      }
      case Invalid(v, es1) =>
        if (shortCircuit) {
          Invalid(Seq(v),es1)
        } else {
          others match {
            case Valid(vs2, rs2) =>
              Invalid(v +: vs2, es1)
            case Invalid(vs2, es2) =>
              Invalid(v +: vs2, es1 ++ es2)
          }
        }
    }
  */  
}


