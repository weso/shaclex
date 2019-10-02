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

  def cnvMapMap[A, B, C, A1, B1, C1](mm: Map[A, Map[B,C]],
                            cnvKeys1: A => A1,
                            cnvKeys2: B => B1,
                            cnvValues: C => C1
                           ): Map[A1, Map[B1,C1]] = {
    def valuesMap(m1: Map[B,C]): Map[B1,C1] =
      cnvMap(m1,cnvKeys2, cnvValues)
    cnvMap(mm,cnvKeys1,valuesMap)
  }

}