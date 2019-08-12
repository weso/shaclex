package es.weso.rbe.interval

import cats._
import org.scalactic._
import scala.math.{ max => intMax }
import scala.math.{ min => intMin }

case class IntOrUnboundedException(msg: String)
  extends Exception("IntOrUnbounded: " + msg)

/**
 * Represents a limit of an [[Interval interval]].
 * It can be either an Int or an Unbounded value
 */
sealed abstract trait IntOrUnbounded {

  /**
   * `true` if this value is Unbounded
   */
  def isUnbounded: Boolean

  /**
   * Add another IntOrUnbounded to this value
   *
   */
  def +(other: => IntOrUnbounded): IntOrUnbounded

  def minusOne: IntOrUnbounded

  def hasLimit = !isUnbounded

  def getLimit: Option[Int]

  def max(other: => IntOrUnbounded): IntOrUnbounded = {
    this match {
      case Unbounded => Unbounded
      case IntLimit(v1) => other match {
        case Unbounded => Unbounded
        case IntLimit(v2) => IntLimit(intMax(v1, v2))
      }
    }
  }

  def min(other: => IntOrUnbounded): IntOrUnbounded = {
    this match {
      case Unbounded => other
      case IntLimit(v1) => other match {
        case Unbounded => this
        case IntLimit(v2) => IntLimit(intMin(v1, v2))
      }
    }
  }

  def show: String = {
    this match {
      case Unbounded => "-"
      case IntLimit(v) => v.toString
    }
  }

  implicit def intOrUnboundedShow: Show[IntOrUnbounded] = {
    Show.show(_.show)
  }

  override def toString = show

  def >(other: IntOrUnbounded): Boolean = {
    (this, other) match {
      case (Unbounded, Unbounded) => false
      case (Unbounded, IntLimit(_)) => true
      case (IntLimit(_), Unbounded) => false
      case (IntLimit(n), IntLimit(m)) => n > m
    }
  }

  def >=(x: Int): Boolean = {
    this match {
      case Unbounded => true
      case IntLimit(m) => m >= x
    }
  }

  def <=(x: Int): Boolean = {
    this match {
      case Unbounded => false
      case IntLimit(m) => m <= x
    }
  }

}

case object Unbounded extends IntOrUnbounded {
  def isUnbounded = true

  def +(other: => IntOrUnbounded): IntOrUnbounded =
    Unbounded

  def minusOne: IntOrUnbounded =
    Unbounded

  def getLimit: Option[Int] = None

}

/**
 * Positive integers
 */
case class IntLimit(m: Int) extends IntOrUnbounded with Requirements {

  require(m >= 0)

  def isUnbounded = false

  def +(other: => IntOrUnbounded): IntOrUnbounded =
    other.getLimit match {
      case None => Unbounded
      case Some(n) => IntLimit(m + n)
    }

  def minusOne: IntOrUnbounded = {
    if (m > 0) IntLimit(m - 1)
    else this
  }

  def getLimit: Option[Int] = Some(m)
}

object IntOrUnbounded {

  def apply(o: Option[Int]): IntOrUnbounded = {
    o match {
      case None => Unbounded
      case Some(n) => IntLimit(n)
    }
  }

  implicit def int2LimitInt(x: Int) = IntLimit(x)

  private def divIntUp(x: Int, y: Int): Int = {
    (x + y - 1) / y
  }

  private def divIntDown(x: Int, y: Int): Int = {
    x / y
  }

  def divIntLimitDown(x: Int, y: IntOrUnbounded): IntOrUnbounded = {
    (x, y) match {
      case (0, IntLimit(0)) => Unbounded // This is to include 0-up cardinalities. Verify if this right
      case (0, _) => _0
      case (_, Unbounded) => _1
      case (_, IntLimit(0)) => Unbounded
      case (_, IntLimit(m)) => divIntDown(x, m)
    }
  }

  def divIntLimitUp(x: Int, y: IntOrUnbounded): IntOrUnbounded = {
    (x, y) match {
      case (0, _) => _0
      case (_, Unbounded) => _1
      case (_, IntLimit(0)) => Unbounded
      case (_, IntLimit(m)) => divIntUp(x, m)
    }
  }

  lazy val _0 = IntLimit(0)
  lazy val _1 = IntLimit(1)

}

