package es.weso.converter
import cats._, data._
import cats.implicits._

trait Converter {

  type Err = String

  type Result[A] = ValidatedNel[Err, A]

  def ok[A](x: A): Result[A] =
    Validated.valid(x)

  def err[A](msg: String): Result[A] =
    Validated.invalidNel(msg)

  def sequence[A](ls: List[Result[A]]): Result[List[A]] =
    ls.sequence[Result,A]

}