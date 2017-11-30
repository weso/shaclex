package es.weso.shacl.validator

import cats._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.checking.CheckerCats
import es.weso.shacl._
import es.weso.shacl.validator.Validator.ShapeTyping
import es.weso.typing._
import es.weso.shacl.showShacl._

object SHACLChecker extends CheckerCats {

  type Config = RDFReader
  type Env = ShapeTyping
  type Err = ViolationError
  //    type Evidence = (NodeShapePair, String)
  type Log = List[Evidence]
  implicit val envMonoid: Monoid[Env] = new Monoid[Env] {
    def combine(e1: Env, e2: Env): Env = e1.combineTyping(e2)
    def empty: Env = Typing.empty
  }
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log): Log = l1 ++ l2
    def empty: Log = List()
  }
  implicit val logShow: Show[Log] = new Show[Log] {
    def show(l: Log): String = l.map(_.show).mkString("\n")
  }
  implicit val typingShow: Show[ShapeTyping] = new Show[ShapeTyping] {
    def show(t: ShapeTyping): String = Typing.showTyping[RDFNode,Shape,ViolationError,String].show(t)
  }
}
