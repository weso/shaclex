package es.weso.shex.validator

import cats._
import cats.implicits._

case class CandidateLine(values: List[(Arc,ConstraintRef)])

object CandidateLine {

  implicit lazy val showCandidateLine = new Show[CandidateLine] {
    def show(cl: CandidateLine): String = {
      s"[${cl.values.map{ case (arc,cref) => (arc.show, cref.show)}.mkString(",")}]"
    }
  }

}
