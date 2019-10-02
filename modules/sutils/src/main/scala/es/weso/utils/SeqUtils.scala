package es.weso.utils
import cats._
import implicits._

import scala.annotation.tailrec

object SeqUtils {

  def zipN[A](s: List[List[A]]): List[List[A]] = {
    def f(x: List[A], rest: List[List[A]]): List[List[A]] = {
      if (x.isEmpty) rest
      else for {
        v <- x
        r <- rest
      } yield v :: r
    }
    s.foldRight(List(List[A]()))(f)
  }

  /**
   * transpose(List(("A",List(1,2)), ("B",List(2,3)),("C",List(4)))) =
   *    List(List(("A",1),("B",2),("C",4)),
   *         List(("A",1),("B",3),("C",4)),
   *         List(("A",2),("B",2),("C",4)),
   *         List(("A",2),("B",3),("C",4)))
   * TODO: Generalize this function using a Monoid to return a Stream...
   * @param ls
   * @tparam A
   * @tparam B
   * @return
   */
  def transpose[A, B](ls: List[(A, Set[B])]): List[List[(A, B)]] = {
    val as: List[A] = ls.map(_._1)
    val sequences: List[List[B]] = ls.map(_._2.toList).sequence
    for {
      s <- sequences
    } yield as.zip(s)
  }

  /**
    * filterOptions((1,Some('a')),(2,None),(3,Some('c'))) = List((1,'a'),(3,'c'))
    * @param ls
    * @tparam A
    * @tparam B
    * @return
    */
  def filterOptions[A, B](ls: List[(A, Option[B])]): List[(A, B)] = {
    ls.collect { case (x, Some(y)) => (x,y )}
  }

  /**
   * Similar to Haskel's intersperse
   * intersperse(",",List("A","B","C") = "A,B,C"
   * @param a
   * @param xs
   * @tparam A
   * @return
   */
  def intersperse[A](a: A, xs: Seq[A]): Seq[A] = {
    @tailrec
    def intersperse0(accum: Seq[A], rest: Seq[A]): Seq[A] = rest match {
      case Nil => accum
      case x :: Nil => x +: accum
      case h :: t => intersperse0(a +: h +: accum, t)
    }
    intersperse0(Nil, xs).reverse
  }

}