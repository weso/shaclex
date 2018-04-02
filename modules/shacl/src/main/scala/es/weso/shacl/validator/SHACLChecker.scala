package es.weso.shacl.validator

import cats._
import cats.implicits._
import es.weso.rdf._
import es.weso.checking.CheckerCats
import es.weso.shacl.report.ValidationResult

object SHACLChecker extends CheckerCats {

  type Config = RDFReader
  type Env = ShapeTyping
  type Err = ValidationResult
  type Log = List[Evidence]
  implicit val envMonoid: Monoid[Env] = Monoid[ShapeTyping]
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log): Log = l1 ++ l2
    def empty: Log = List()
  }
  implicit val logShow: Show[Log] = new Show[Log] {
    def show(l: Log): String = l.map(_.show).mkString("\n")
  }
}
