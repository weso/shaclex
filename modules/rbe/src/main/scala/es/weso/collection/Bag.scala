package es.weso.collection

import scala.collection.SortedMap

trait Bag[A] {

  /**
   * Multiplicity of an element in a bag
   */
  def multiplicity(elem: A): Int

  /**
   * Check if a bag contains an element
   */
  def contains(elem: A): Boolean

  /**
   * Inserts an element in a bag and returns the new bag
   */
  def insert(elem: A): Bag[A]

  /**
   * Deletes an element and returns the new bag
   * It doesn't complain if the element doesn't exist
   */
  def delete(elem: A): Bag[A]

  /**
   * Elements of a Bag with their multiplicity
   */
  def elems: Iterator[(A, Int)]

  /**
   * add an element n times to a bag
   * This default implementation is not efficient, it should be overridden
   */
  def add(elem: A, n: Int): Bag[A] =
    (1 to n).toList.foldLeft(this)((s, _) => s.insert(elem))

  /**
   * Union of two bags
   */
  def union(other: Bag[A]): Bag[A] =
    other.elems.toList.foldLeft(this)((s, p) => s.add(p._1, p._2))

  /*
   * TODO: Add intersection of two bags
   *
  def intersection(other: Bag[A]): Bag[A] = {
    val zero : Bag[A] = Bag.empty // type error implicit Ordering not found...
    zero
  } */

  /**
   * Size returns the total number of elements.
   * Notice that if an element appears 4 times, it adds 4 to the counter
   */
  def size: Int = {
    elems.foldLeft(0)((r, n) => r + n._2)
  }

  override def equals(other: Any): Boolean = {
    other.isInstanceOf[Bag[A]] &&
      this.asSortedMap.equals(other.asInstanceOf[Bag[A]].asSortedMap)
  }

  def asSortedMap: SortedMap[A, Int]

  def from(t: Traversable[A]): Bag[A] = {
    t.foldLeft(this)((s, a) => s.insert(a))
  }

  def toSeq: Seq[A] = {
    def generate(x: A, n: Int): Seq[A] = {
      (1 to n).map(_ => x)
    }

    this.asSortedMap.map(
      pair => generate(pair._1, pair._2)).flatten.toSeq
  }
}

object Bag {

  def apply[A: Ordering](as: A*): Bag[A] = {
    Bag.toBag(as.toList)
  }

  def empty[A: Ordering]: Bag[A] =
    BagSortedMap(SortedMap[A, Int]())

  def toBag[A: Ordering](t: Traversable[A]): Bag[A] = {
    empty.from(t)
  }

  /**
   * Calculates the bag obtained by taking into account only a set of symbols
   */
  def delta[A: Ordering](symbols: Seq[A], bag: Bag[A]): Bag[A] = {
    val e: Bag[A] = empty
    symbols.foldLeft(e)((rest, x) => rest.add(x, bag.multiplicity(x)))
  }

}