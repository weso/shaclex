package es.weso.utils

import scala.util.{Either, Left, Right}
import cats.implicits._


object EitherUtils {

  /** Although we could use cats sequence method directly,
    * IntelliJ raises an ugly false error in the code,
    * so we encapsulate the error in a single place
    * */
  def sequence[A,E](ds: List[Either[E,A]]): Either[E, List[A]] = {
    type ES[V] = Either[E,V]
    ds.sequence[ES,A]
  }

  def takeSingle[A](ls: List[A], msg: String): Either[String,A] =
    if (ls.length == 1) Right(ls.head)
    else Left(msg)

}