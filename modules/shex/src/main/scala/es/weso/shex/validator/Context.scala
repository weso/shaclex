package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.shex.VarTable

case class Context(typing: ShapeTyping, varTable: VarTable)

object Context {
  implicit val ctxMonoid: Monoid[Context] = new Monoid[Context] {
    def combine(e1: Context, e2: Context): Context =
      Context(e1.typing |+| e2.typing, e1.varTable |+| e2.varTable)

    def empty: Context = {
      Context(Monoid[ShapeTyping].empty, Monoid[VarTable].empty)
    }
  }



  def fromTyping(typing: ShapeTyping): Context = Context(typing, Monoid[VarTable].empty)

  def updateTyping(c: Context, f: ShapeTyping => ShapeTyping): Context =
    c.copy(typing = f(c.typing))

}