package es.weso.utils

object SetUtils {


  /**
    * paritition of a set in n sets whose union give that set again
    * Example:
    * partition(Set(1,2),2) = Stream(
    *   List(Set(1, 2), Set[Int]()),
    *   List(Set(2), Set(1)),
    *   List(Set(1), Set(2)),
    *   List(Set[Int](), Set(1, 2))
    * )
    *
    * partition(Set(1,2),3) = Stream(
    *  List(Set(1, 2), Set[Int](), Set[Int]()),
    *  List(Set(2), Set(1), Set[Int]()),
    *  List(Set(2), Set[Int](), Set(1)),
    *  List(Set(1), Set(2), Set[Int]()),
    *  List(Set(1), Set[Int](), Set(2)),
    *  List(Set[Int](), Set(1, 2), Set[Int]()),
    *  List(Set[Int](), Set(2), Set(1)),
    *  List(Set[Int](), Set(1), Set(2)),
    *  List(Set[Int](), Set[Int](), Set(1, 2))
    * )
    *
    * @param set set of elements
    * @param n number of sets to generate
    * @tparam A type of values
    * @return a list of sets
    */
 def partition[A](set: Set[A], n: Int): LazyList[List[Set[A]]] = n match {
   case 1 => LazyList(List(set))
   case n if n > 1 => for {
     pair <- pSet(set)
     (s1,s2) = pair
     rest <- partition(s2, n - 1)
   } yield List(s1) ++ rest
   case _ => throw new Exception(s"partition invoked with wrong n: $n")
 }

  /**
    *  Tail recursive pSet
    * pSet s generates the power set of s, pairing each subset with its complement.
    *  Example: pSet(Set(1,2)) =
    *       Stream((Set(1,2),Set()),(Set(1),Set(2)),(Set(2),Set(1)),(Set(),Set(1,2))).
    **/
  def pSet[A](set: Set[A]): LazyList[(Set[A], Set[A])] = {

    @annotation.tailrec
    def pSetRec(set: Set[A],
                acc: LazyList[(Set[A], Set[A])]): LazyList[(Set[A], Set[A])] = {
      if (set.isEmpty) acc
      else {
        val x = set.head
        pSetRec(set.tail, acc.map(addFirst(x)) ++ acc.map(addSecond(x)))
      }
    }
    pSetRec(set, LazyList((Set(), Set())))
  }

  private def addFirst[A](x: A)(pair: (Set[A], Set[A])): (Set[A], Set[A]) = {
    (pair._1 + x, pair._2)
  }

  private def addSecond[A](x: A)(pair: (Set[A], Set[A])): (Set[A], Set[A]) = {
    (pair._1, pair._2 + x)
  }

}