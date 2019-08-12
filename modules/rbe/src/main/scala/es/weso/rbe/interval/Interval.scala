package es.weso.rbe.interval

import IntOrUnbounded._
import cats.Show

case class IntervalsException(msg: String)
  extends Exception(msg)

/**
 * Definition of intervals (m,n) where m and n can be unbounded
 */
case class Interval(n: IntOrUnbounded, m: IntOrUnbounded) {

  /**
   * checks if an interval is empty
   */
  def isEmpty: Boolean = {
    n > m
  }

  /**
   * interval addition
   */
  def +(other: Interval): Interval = {
    Interval(
      n = n + other.n,
      m = m + other.m)
  }

  /**
   * interval intersection
   */
  def &(other: Interval): Interval = {
    Interval(
      n = n.max(other.n),
      m = m.min(other.m))
  }

  private def show: String = m.getLimit match {
    case None => s"[${n.show};-]"
    case Some(mlimit) =>
      if (n > mlimit)
        s"<empty(${n.show};${m.show})>"
      else
        s"[${n.show};${m.show}]"
  }

  implicit def limitShow: Show[IntOrUnbounded] = {
    Show.show(_.show)
  }

  override def toString = show

  /**
   * Checks if a value belongs to an interval
   */
  def contains(v: Int) = {
    n <= v && m >= v
  }

}

