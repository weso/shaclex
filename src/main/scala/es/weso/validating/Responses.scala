package es.weso.validating

import cats.Functor
import cats.implicits._

/**
 * Represents a non-deterministic response which can contains several 
 * values and reasons
 */
case class Responses[A,R[_]: Functor](values: Seq[Response[A,R]]) {
  
/*  def combine[B](other:Responses[B,R])(f: A => B): Responses[B,R] = {
    ???
  } */
  
  /**
   * Concatenate two responses appending the values and reasons
   */
  def ++(other: Responses[A,R]): Responses[A,R] = {
    Responses(values ++ other.values)
  }
  
  def map[B](f: A => B): Responses[B,R] = {
    Responses(values.map(r => r.mapValue(f)))
  }
  
  def combineWith(
      other: Responses[A,R], 
      f: (Response[A,R],Response[A,R]) => Response[A,R]): Responses[A,R] = {
    if (values.isEmpty) other
    else if (other.values.isEmpty) this
    else {
    val rs = for {
     v1 <- values;
     v2 <- other.values
    } yield f(v1,v2)
    Responses(rs)
    }
  }
  
  def mapResponse(f: Response[A,R] => Response[A,R]): Responses[A,R] = {
    Responses(values.map(f))
  }

  override def toString: String = {
    values.toString
  }
}

object Responses {
  
  def single[A,R[_]:Functor](r:R[A]): Responses[A,R] = {
    Responses(Seq(Response(r)))
  }
  
  def empty[A,R[_]:Functor]: Responses[A,R] = Responses(Seq())
}
