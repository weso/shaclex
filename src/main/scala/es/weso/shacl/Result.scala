package es.weso.shacl
import cats._, data._
import cats.syntax.show
import cats.std.list._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._

case class CheckResult[
  E: Show,
  A: Show]
  (r: Xor[NonEmptyList[E],List[A]]) {
  
  def isOK: Boolean = r.isRight && hasResults(r)

  // Is there a better way to define this?
  def hasResults(r: Xor[NonEmptyList[E],List[A]]): Boolean =
    r.fold(_ => false, xs => !xs.isEmpty)
  
  def errors: Seq[E] = 
    r.fold(_.unwrap, _ => Seq())
    
  def results: List[A] = {
    r.fold(_ => List(), x => x)
  }
  
  def show: String = {
    if (isOK) {
      val first = results.head
      "OK. First result: " ++ "\n" ++ 
      Show[A].show(first)
    } else
      "Not OK. Errors: " ++ "\n" ++ errors.map(e => Show[E].show(e)).mkString("\n")
  }
}

