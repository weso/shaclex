package es.weso.validating

import cats._
import cats.implicits._

case class Response[A,R[_]:Functor](response: R[A]) {
  
  def mapValue[B](f: A => B): Response[B, R] = {
    Response(response = implicitly[Functor[R]].map(response)(f))  
  }
  
  def merge(rs: Response[Seq[A],R]): Response[Seq[A],R] = {
    val rs1 : R[Seq[A]] = mapValue(x => Seq(x)).response
    val rs2 : R[Seq[A]] = rs.response
    ???
  }
  
}

object Response {

}
