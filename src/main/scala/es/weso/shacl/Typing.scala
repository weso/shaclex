package es.weso.shacl
import es.weso.rdf.nodes._
import cats._, data._
import cats.implicits._

case class TypingResult(t: Xor[NonEmptyList[ViolationError],Actions]) extends AnyVal {
  def isOK = t.isRight
} 

case class Typing(m: Map[RDFNode, Map[Shape,TypingResult]]) {
  
  def getShapes(node: RDFNode): Map[Shape,TypingResult] = 
    m.get(node).getOrElse(Map())
  
  def getOkShapes(node: RDFNode): Set[Shape] = {
    getShapes(node).filter(p => p._2.isOK).keySet
  }
  
  def getFailedShapes(node: RDFNode): Set[Shape] = {
    getShapes(node).filter(p => !p._2.isOK).keySet
  }
  
  def getResult(node: RDFNode, shape: Shape): Option[TypingResult] = {
    m.get(node) match {
      case None => None
      case Some(mm) => mm.get(shape)
    }
  }
  
  def firstAction(msg: String): TypingResult = {
    val a: Xor[NonEmptyList[ViolationError],Actions] = Actions.initial(msg).right
    TypingResult(a)
  }
    
  def addAction(nodeShape: NodeShape, msg: String): Typing = {
    val node = nodeShape.node
    val shape = nodeShape.shape
    getResult(node,shape) match {
      case None => Typing(m.updated(node, Map(shape -> firstAction(msg)))) 
      case Some(current) =>
        if (current.isOK) {
          val newAs = 
            TypingResult(current.t.fold(_ => Actions.empty, as => as.addAction(msg)).right)
          Typing(m.updated(node,m(node).updated(shape,newAs)))
        } else
          this
    }
  } 
  
  def showResult(r: TypingResult): String = {
    r.t.fold(es => "Incorrect. Errors: \n " ++ es.unwrap.map(_.toString).mkString("\n "),
             as => "Correct. Reasons: \n " ++ as.strs.mkString("\n "))
  }
  
  override def toString: String = {
    m.map(t => {
      val node = t._1
      val shapesMap = t._2
      shapesMap.map(s => {
        val shape = s._1.id.getOrElse("_?")
        val result = s._2
        val showShape = (if (result.isOK) "+" else "-" ) + shape 
        s"($node - $showShape) -> ${showResult(result)}"
      }).mkString("\n")
    }).mkString("\n")
  }
}

object Typing {
  def empty = Typing(Map())
}

