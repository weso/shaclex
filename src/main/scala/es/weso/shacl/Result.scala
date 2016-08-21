package es.weso.shacl
import cats._, data._
import cats.syntax.show
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._

case class CheckResult[A: Show]
  (r: Xor[Xor[Throwable,String],A]) {
  
  def isOK: Boolean = r.isRight 

  def result: Option[A] = {
    r.fold(_ => None, x => Some(x))
  }
  
  def show: String = {
    if (isOK) {
      val first = result.head
      "OK. Result: " ++ "\n" ++ 
      Show[A].show(first)
    } else
      "Not OK. Error: " + r
  }
}

