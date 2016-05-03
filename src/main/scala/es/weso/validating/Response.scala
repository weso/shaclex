package es.weso.validating

import cats.Functor
import cats.implicits._

case class Response[A,R[_]:Functor](response: R[A]) {
  
  def mapValue[B](f: A => B): Response[B, R] = {
    Response(response = implicitly[Functor[R]].map(response)(f))  
  }
  
}

object Response {

 /*implicit def responseFunctor[A,R[_]:Functor, ] = new Functor[Response] {
   def map[B](fa: Response[A,R])(f:A => B): Response[B,R] = {
    fa match {
      case SomeReason(cs) => SomeReason(cs.map(c => this.map(c)(f)))
      case AllReason(cs) => AllReason(cs.map(c => this.map(c)(f)))
      case OneOfReason(cs) => OneOfReason(cs.map(c => this.map(c)(f)))
      case SingleReason(x,msg) => SingleReason(f(x),msg)
    }
  } 
  } */
}
