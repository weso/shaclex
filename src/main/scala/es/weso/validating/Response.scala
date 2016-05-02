package es.weso.validating

import cats.Functor
import cats.implicits._


case class Response[A,R[_]:Functor](response: R[A]) {
  
  def mapValue[B](f: A => B): Response[B, R] = {
    Response(response = implicitly[Functor[R]].map(response)(f))  
  }
}
