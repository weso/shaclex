package es.weso.validating

import cats._
import cats.implicits._

/**
 * Represents a non-deterministic response which can contains several
 * values and reasons
 */
case class Responses[A, R[_]: Applicative](values: Seq[Response[A, R]]) {

  /**
   * Merge this set of responses with another one that contains list of values
   */
  def merge(rs: Responses[Seq[A], R]): Responses[Seq[A], R] = {
    if (values.isEmpty) rs
    else {
     val rss = values.map(ra => Responses.add(ra,rs))
     Responses.flatten(rss)
    }
  }

  /**
   * Concatenate two responses appending the values and reasons
   */
  def ++(other: Responses[A, R]): Responses[A, R] = {
    Responses(values ++ other.values)
  }

  def map[B](f: A => B): Responses[B, R] = {
    Responses(values.map(r => r.mapValue(f)))
  }

  def combineWith(
    other: Responses[A, R],
    f: (Response[A, R], Response[A, R]) => Response[A, R]): Responses[A, R] = {
    if (values.isEmpty) other
    else if (other.values.isEmpty) this
    else {
      val rs = for {
        v1 <- values;
        v2 <- other.values
      } yield f(v1, v2)
      Responses(rs)
    }
  }

  def mapResponse(f: Response[A, R] => Response[A, R]): Responses[A, R] = {
    Responses(values.map(f))
  }

  override def toString: String = {
    values.toString
  }
}

object Responses {

  def single[A, R[_]: Applicative](r: R[A]): Responses[A, R] = {
    Responses(Seq(Response(r)))
  }

  def initial[A, R[_]: Applicative]: Responses[A, R] = Responses(Seq())
  
  def add[A,R[_]:Applicative](
        ra: Response[A, R], 
        rsa: Responses[Seq[A], R]): Responses[Seq[A], R] = {
    if (rsa.values.isEmpty) {
      val rs: Response[Seq[A],R] = ra.mapValue(x => Seq(x))
      Responses(Seq(rs))
    } else {
      Responses(rsa.values.map(rs => ra.merge(rs)))
    }
  }
  
  def flatten[A,R[_]:Applicative](rss: Seq[Responses[A,R]]): Responses[A,R] = {
    val zero: Responses[A,R] = initial
    def next(x: Responses[A,R], r: Responses[A,R]): Responses[A,R] = x ++ r
    rss.foldRight(zero)(next)
  }
}
