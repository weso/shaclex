package es.weso.utils
import cats._
import implicits._

object MapUtils {

  def combineMaps[A](ls: List[Map[A, Int]]): Map[A, Int] = {
    ls.foldLeft(Map[A, Int]())((x, y) => x.combine(y))
  }

  def cnvMap[A, B, C, D](m: Map[A, B], cnvKeys: A => C, cnvValues: B => D): Map[C, D] = {
    m.map { case (a, b) => (cnvKeys(a), cnvValues(b)) }
  }

}