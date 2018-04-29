package es.weso.shex.validator

import cats._
import implicits._
import es.weso.rdf.nodes.IRI

case class Action(name: IRI, code: Option[String])

object Action {

  implicit val actionShow: Show[Action] = new Show[Action] {
    def show(a: Action): String = s"Action[${a.name.show}] - ${a.code.getOrElse("")}"
  }

}