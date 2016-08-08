package es.weso.shacl
import es.weso.rdf.nodes._
import cats._, data._
import cats.implicits._

case class Typing(m: Map[NodeShape,Xor[NonEmptyList[ViolationError],Actions]]) {
  def addAction(nodeShape: NodeShape, msg: String): Typing = {
    if (m.contains(nodeShape)) {
      val current = m(nodeShape)
      if (current.isRight) {
        val newAs = current.fold(_ => Actions.empty, as => as.addAction(msg))
        Typing(m.updated(nodeShape,newAs.right))
      } else this // TODO: raise some error?
    } else
      Typing(m.updated(nodeShape, Actions.initial(msg).right))
  }
  
  def showResult(r: Xor[NonEmptyList[ViolationError],Actions]): String = {
    r.fold(es => "Incorrect. Errors: \n " ++ es.unwrap.map(_.toString).mkString("\n "),
           as => "Correct. Reasons: \n " ++ as.strs.mkString("\n "))
  }
  
  override def toString: String = {
    m.map(t => {
      val nodeShape = t._1
      val result = t._2
      s"(${nodeShape.node} - ${nodeShape.shape.id.getOrElse("?")}) -> ${showResult(result)}"
    }).mkString("\n")
  }
}

object Typing {
  def empty = Typing(Map())
}

