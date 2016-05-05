package es.weso.validating

import cats._
import cats.implicits._

/**
 * Represents a non-deterministic response which can contains several
 * values and reasons
 */
case class Responses[A, R[_]: Functor](values: Seq[Response[A, R]]) {

  /*  def combine[A,R[_]:Applicative](other:Responses[B,R])(f: A => B): Responses[B,R] = {
    ???
  } */

  def merge(rss: Responses[Seq[A], R]): Responses[Seq[A], R] = {
    val zero: Responses[Seq[A], R] = Responses(Seq())
    def next(v: Response[A, R],
             rest: Responses[Seq[A], R]): Responses[Seq[A], R] = {
      val z: Seq[Response[Seq[A], R]] = Seq()
      def n(x: Response[Seq[A], R], cs: Seq[Response[Seq[A], R]]): Seq[Response[Seq[A], R]] = x +: cs
      Responses(rest.values.foldRight(z)(n))
    }
    values.foldRight(zero)(next)
  }

/*  def merge1(vs: Response[Seq[A], R]): Responses[Seq[A], R] = {
    def fn(r: Response[A, R]): Response[Seq[A], R] = {
      r merge vs
    }
    Responses(values.map(fn))
  } */

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

  def single[A, R[_]: Functor](r: R[A]): Responses[A, R] = {
    Responses(Seq(Response(r)))
  }

  def initial[A, R[_]: Functor]: Responses[A, R] = Responses(Seq())
}
