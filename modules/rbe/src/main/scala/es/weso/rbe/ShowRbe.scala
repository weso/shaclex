package es.weso.rbe
import cats._
import data._
import es.weso.rbe.interval.{ IntLimit, IntOrUnbounded, Unbounded }
import implicits._

object ShowRbe {

  implicit final val showIntOrUnbounded: Show[IntOrUnbounded] = new Show[IntOrUnbounded] {
    def show(m: IntOrUnbounded): String = m match {
      case Unbounded => "_"
      case IntLimit(m) => m.show
    }
  }

  implicit def showRbe[A: Show]: Show[Rbe[A]] = new Show[Rbe[A]] {
    def show(rbe: Rbe[A]): String = rbe match {
      case Fail(msg) => s"Fail($msg)"
      case Empty => "{}"
      case Symbol(a, n, m) => s"${a.show}{$n,${m.show}"
      case And(v1, v2) => s"${show(v1)},${show(v2)}"
      case Or(v1, v2) => s"${show(v1)}|${show(v2)}"
      case Star(v) => s"${show(v)}*"
      case Plus(v) => s"${show(v)}+"
      case Repeat(v, n, m) => s"(${show(v)}){$n,${m.show}"
    }
  }
}
