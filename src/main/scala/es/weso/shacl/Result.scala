package es.weso.shacl
import cats._, data._
import cats.syntax.show
import cats.std.list._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._

case class CheckResult(r: Xor[NonEmptyList[ViolationError],List[(Schema,Evidences)]]) {
  def isOK: Boolean = r.isRight && hasResults(r)

  // Is there a better way to define this?
  def hasResults[E,A](r: Xor[NonEmptyList[E],List[A]]): Boolean =
    r.fold(_ => false, xs => !xs.isEmpty)
  
  def errors: Seq[ViolationError] = 
    r.fold(_.unwrap, _ => Seq())
    
  def results: List[(Schema,Evidences)] = {
    r.fold(_ => List(), x => x)
  }
  
  def show: String = {
    if (isOK) {
      val typing = results.head._2
      "Valid. Typing: " ++ "\n" ++ 
      typing.toString
    } else
      "Not valid. Errors: " ++ "\n" ++ errors.map(e => e.toString).mkString("\n")
  }
}

