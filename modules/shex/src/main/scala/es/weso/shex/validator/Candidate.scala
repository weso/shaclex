package es.weso.shex.validator

import cats.Show
import cats.implicits._

case class Candidate(arc: Arc, crefs: Set[ConstraintRef]) {
  override def toString: String = Candidate.showCandidate.show(this)
}

object Candidate {

 implicit lazy val showCandidate = new Show[Candidate] {
   override def show(c:Candidate): String = {
     s"${c.arc.show} ~ ${c.crefs.map(_.show).mkString(" ")} "
   }
 }

}

