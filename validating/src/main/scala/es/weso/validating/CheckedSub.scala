package es.weso.validating
import cats.{Monoid, Semigroup}

/**
 * Checked implementation using Subtyping
 */
object CheckedSub extends CheckedOps {

  case class Valid[A, R, E](
      value: Option[A], 
      responses: Seq[R]) extends Checked[A, R, E] {

    override def isOK = true

    override def errors = Seq()

    override def mapValue[B](f: A => B): Checked[B, R, E] = this.copy(value = value.map(f))

    override def fold[V](oks: (Option[A], Seq[R]) => V, errs: (Option[A], Seq[E]) => V): V = {
      oks(value, responses)
    }

    override def mapErrors[E1](f: E => E1): Checked[A, R, E1] =
      Valid(
        value = this.value,
        responses = this.responses)

    override def addErrors(es: Seq[E]): Checked[A, R, E] = Invalid(value, es)

  }

  case class Invalid[A, R, E](
      value: Option[A],
      errors: Seq[E]) extends Checked[A, R, E] {

    require(!errors.isEmpty)

    override def isOK = false

    override def mapValue[B](f: A => B): Checked[B, R, E] = this.copy(value = value.map(f))

    override def responses = Seq()

    override def fold[V](oks: (Option[A], Seq[R]) => V, errs: (Option[A], Seq[E]) => V): V = {
      errs(value, responses)
    }

    override def mapErrors[E1](f: E => E1): Checked[A, R, E1] =
      this.copy(errors = this.errors.map(f(_)))

    override def addErrors(es: Seq[E]): Checked[A, R, E] = this.copy(errors = errors ++ es)

  }

 def err[A, R, E](v: Option[A], e: E): Checked[A, R, E] =
    Invalid(v, Seq(e))

 def errs[A, R, E](v: Option[A], es: Seq[E]): Checked[A, R, E] = {
    Invalid(v, es)   
 }
    
 def okZeroSeq[A, R, E](): Checked[Seq[A], R, E] =
    Valid(Some(Seq()), Seq())
    
 def ok[A, R, E](x: Option[A], r: R): Checked[A, R, E] =
    Valid(x, Seq(r))
    
 def oks[A, R, E](v: Option[A], rs: Seq[R]): Checked[A, R, E] =
    Valid(v, rs)
    
 def mergeSeq[R: Semigroup](
      rs1: Seq[R], 
      rs2: Seq[R]): Seq[R] = {
    if (rs1.isEmpty) rs2
    else if (rs2.isEmpty) rs1 
    else
    for (r1 <- rs1; r2 <- rs2)
        yield Semigroup[R].combine(r1,r2)
  }   

  def checkAll[A, R:Semigroup, E](
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

  def join[A](x: Option[A], xs: Option[Seq[A]]): Option[Seq[A]] = {
    (x,xs) match {
      case (None,None) => None
      case (None, Some(xs)) => Some(xs)
      case (Some(x), None) => Some(Seq(x))
      case (Some(x), Some(xs)) => Some(x +: xs)
    }
  }
  
  def merge[A, R: Semigroup, E](
    first: Checked[A, R, E],
    others: => Checked[Seq[A], R, E],
    shortCircuit: Boolean = true
  ): Checked[Seq[A], R, E] =
    first match {
      case Valid(v, rs1) => others match {
        case Valid(vs2, rs2) => Valid(join(v,vs2), mergeSeq(rs1,rs2))
        case Invalid(vs2, es) => Invalid(join(v,vs2), es)
      }
      case Invalid(v, es1) =>
        if (shortCircuit) {
          Invalid(v.map(Seq(_)),es1)
        } else {
          others match {
            case Valid(vs2, rs2) =>
              Invalid(join(v,vs2), es1)
            case Invalid(vs2, es2) =>
              Invalid(join(v,vs2), es1 ++ es2)
          }
        }
    }
}


