package es.weso.utils
import cats._, data._
import implicits._

object MapUtils {

  def combineMaps[A](ls: List[Map[A, Int]]): Map[A, Int] = {
    ls.foldLeft(Map[A, Int]())((x, y) => x.combine(y))
  }
}