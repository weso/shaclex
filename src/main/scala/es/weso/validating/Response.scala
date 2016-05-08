package es.weso.validating

import cats._
import cats.implicits._

case class Response[A,R[_]:Applicative](response: R[A]) {
  
  def mapValue[B](f: A => B): Response[B, R] = {
    Response(response = implicitly[Functor[R]].map(response)(f))  
  }
  
  def merge(responses: Response[Seq[A],R]): Response[Seq[A],R] = {
    val r : R[A] = response
    val rs : R[Seq[A]] = responses.response
    def add(p:(A,Seq[A])):Seq[A] = p._1 +: p._2
    val p : R[Seq[A]] = r.product(rs).map(add)
    Response(p)
  }
  
  override def toString:String = {
    s"Response[$response]"
  }
}

object Response {

  def add[A,R[_]:Applicative](
      rs1: Response[Seq[A],R], rs2: Response[Seq[A],R]): Response[Seq[A],R] = {
    val r1 : R[Seq[A]] = rs1.response
    val r2 : R[Seq[A]] = rs2.response
    def add(p:(Seq[A],Seq[A])):Seq[A] = p._1 ++ p._2
    val p : R[Seq[A]] = r1.product(r2).map(add)
    Response(p)
  }
}
