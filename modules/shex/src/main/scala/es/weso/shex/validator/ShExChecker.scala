package es.weso.shex.validator

import cats._, data._
import implicits._
import es.weso.checking.CheckerCats
import es.weso.rdf.RDFReader
import es.weso.shex.ViolationError

object ShExChecker extends CheckerCats {

  type Config = RDFReader
  type Env = ShapeTyping
  type Err = ViolationError
  type Evidence = (NodeShape, String)
  type Log = List[Evidence]

  import ShapeTyping._
  implicit val envMonoid: Monoid[Env] = new Monoid[Env] {
    def combine(e1: Env, e2: Env): Env = e1 |+| e2
    def empty: Env = Monoid[ShapeTyping].empty
  }
  /*    implicit val logCanLog: CanLog[Log] = new CanLog[Log] {
        def log(msg: String): Log =
          throw new Exception("Not implemented logCanlog")
      } */
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log): Log = l1 ++ l2
    def empty: Log = List()
  }
  implicit val logShow: Show[Log] = new Show[Log] {
    def show(l: Log): String = l.map { case (ns, msg) => s"${ns}: $msg" }.mkString("\n")
  }
  implicit val typingShow: Show[ShapeTyping] = new Show[ShapeTyping] {
    def show(t: ShapeTyping): String = t.toString
  }
}
