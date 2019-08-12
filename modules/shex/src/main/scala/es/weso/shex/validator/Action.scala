package es.weso.shex.validator

import cats._
import implicits._
import es.weso.rdf.nodes.IRI

/**
* Represents actions to be done
  * @param name represents the kind of action
  * @param code contains the code to be executed
  */
case class Action(name: IRI, code: Option[String])

object Action {

  implicit val actionShow: Show[Action] = new Show[Action] {
    def show(a: Action): String = s"Action[${a.name.show}] - ${a.code.getOrElse("")}"
  }

}