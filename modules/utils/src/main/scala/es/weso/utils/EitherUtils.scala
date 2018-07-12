package es.weso.utils

import scala.util.Either
import cats.implicits._


object EitherUtils {

  /** Although we could use cats sequence method directly,
    * IntelliJ raises an ugly false error in the code,
    * so we encapsulate the error in a single place
    * */
  def sequence[A,E](ds: List[Either[E,A]]): Either[E, List[A]] =
    ds.sequence

}