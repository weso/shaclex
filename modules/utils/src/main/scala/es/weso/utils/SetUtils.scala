package es.weso.utils

import scala.annotation.tailrec


object SetUtils {

  /**
    *  Tail recursive pSet
    * pSet s generates the power set of s, pairing each subset with its complement.
    *       e.g. pSet [1,2] = [([1,2],[]),([1],[2]),([2],[1]),([],[1,2])].
    **/
  def pSet[A](set: Set[A]): Stream[(Set[A], Set[A])] = {

    @annotation.tailrec
    def pSetRec(set: Set[A],
                acc: Stream[(Set[A], Set[A])]): Stream[(Set[A], Set[A])] = {
      if (set.isEmpty) acc
      else {
        val x = set.head
        pSetRec(set.tail, acc.map(addFirst(x)) ++ acc.map(addSecond(x)))
      }
    }
    pSetRec(set, Stream((Set(), Set())))
  }

  private def addFirst[A](x: A)(pair: (Set[A], Set[A])): (Set[A], Set[A]) = {
    (pair._1 + x, pair._2)
  }

  private def addSecond[A](x: A)(pair: (Set[A], Set[A])): (Set[A], Set[A]) = {
    (pair._1, pair._2 + x)
  }

  // TODO: Refactor to be tail recursive
  /* def decompose[A](set: Set[A], n: Int): Stream[List[Set[A]]] = {
    n match {
      case 1 => Stream(List(set))
      case m if m > 1 =>
        decompose(set,m - 1).map{
          case (x::xs) => {
            pSet(x) map { case (s1,s2) => List(s1,s2) ++ xs}
          }
          case Nil => throw new Exception("decompose: Unsupported Nil")
        }.flatten
    }
  } */
}