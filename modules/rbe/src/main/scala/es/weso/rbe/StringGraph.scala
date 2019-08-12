package es.weso.rbe
import es.weso.utils._
import es.weso.rbe.nodeShape._

/**
 *  Simple graphs whose nodes and edges are strings
 *  They are used for testing purposes mainly
 */
trait StringGraph extends Graph[String, String] {
}

object StringGraph {

  implicit val readErr = new Read[RbeError] {
    def read(str: String) = RbeError(str)
  }

  implicit val readString = new Read[String] {
    def read(str: String) = str
  }

  implicit def mkErr = RbeError

  type Pred_ = Pred[String, RbeError, String]
  /**
   * Checks a predicate on a value
   * @param x the value to check
   * @param p the predicate
   * @param name name of the condition to check
   * @return if the value satisfies the predicate, a Checker with an ok value, otherwise the error that results of applying ferr to the name of the condition
   */
  def cond[A](
    x: A,
    p: A => Boolean,
    name: String): CheckVal[A, RbeError, String] = {
    if (p(x)) NodeShape.ok[A, RbeError, String](x, "OK")
    else NodeShape.errString[A, String](s"Failed condition $name on $x")
  }

  lazy val isA: Pred_ =
    Pred("isA")(x =>
      cond(x, (x: String) => x == "a", "eqA"))

  lazy val integer: Pred_ =
    Pred("int")(x =>
      cond(x, (x: String) => x.matches("""\d+"""), "integer"))

  lazy val letter: Pred_ =
    Pred("letter")(x =>
      cond(x, (x: String) => x.matches("""[a-zA-Z]+"""), "letter"))

  lazy val size2: Pred_ =
    Pred("size2")(x =>
      cond(x, (x: String) => x.length == 2, "size2"))

  lazy val one: Pred_ =
    Pred("one")(x =>
      cond(x, (x: String) => x == "1", "== 1"))

  lazy val two: Pred_ =
    Pred("two")(x =>
      cond(x, (x: String) => x == "2", "== 2"))

}
