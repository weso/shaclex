package es.weso.collection

import scala.collection.SortedMap

case class BagSortedMap[A: Ordering](smap: SortedMap[A, Int])
  extends Bag[A] {

  def contains(elem: A): Boolean = smap.contains(elem)

  def insert(elem: A): BagSortedMap[A] =
    if (smap.contains(elem))
      BagSortedMap(smap + (elem -> (smap(elem) + 1)))
    else
      BagSortedMap(smap + (elem -> 1))

  def delete(elem: A): BagSortedMap[A] =
    if (smap.contains(elem)) {
      val n = smap(elem)
      if (n == 1)
        BagSortedMap(smap - elem)
      else
        BagSortedMap(smap + (elem -> (n - 1)))
    } else // TODO: Consider returning some kind of error
      this

  def multiplicity(elem: A): Int = {
    if (smap.contains(elem))
      smap(elem)
    else
      0
  }

  /**
   * A more efficient version of add
   */
  override def add(elem: A, n: Int): BagSortedMap[A] =
    if (smap.contains(elem))
      BagSortedMap(smap + (elem -> (smap(elem) + n)))
    else
      BagSortedMap(smap + (elem -> n))

  def elems: Iterator[(A, Int)] = smap.iterator

  def asSortedMap: SortedMap[A, Int] = smap

  override def toString: String = {
    val b = new StringBuilder
    smap.addString(b, "{| ", ", ", " |}")
    b.toString
  }

}

object BagSortedMap {
}