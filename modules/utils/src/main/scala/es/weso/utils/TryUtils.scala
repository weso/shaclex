package es.weso.utils
import util._

object TryUtils {

  /**
   *  The following code has been taken from this answer:
   *  [[http://stackoverflow.com/questions/15495678/flatten-scala-try]]
   * There may be more effcient solutions (but probably less elegant)
   *
   */
  def filterSuccess[A](xs: Seq[Try[A]]): Try[Seq[A]] =
    Try(xs.map(_.get))

  def combineAll[A, B](
    xs: Seq[A],
    current: B,
    comb: (A, B) => Try[Seq[B]]): Try[Seq[B]] = {
    def zero: Try[Seq[B]] = Try(Seq(current))
    def step(rest: Try[Seq[B]], x: A): Try[Seq[B]] = {
      def next(rs: Seq[B]): Try[Seq[B]] = {
        val results = rs.map(r => comb(x, r))
        val fs = filterSuccess(results)
        fs.map(xss => xss.flatten)
      }
      rest.flatMap(next)
    }
    xs.foldLeft(zero)(step)
  }
}

