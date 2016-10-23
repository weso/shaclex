package es.weso.typing

import util._

case class PosNegTypingAsMap[Node,Label](
    m: Map[Node,TypeRow[Label]]) 
  extends PosNegTyping[Node,Label] {
  
  override def nodes = m.keys.toSeq
  
  override def addPosType(node: Node,label: Label): Try[PosNegTypingAsMap[Node,Label]] = {
    val typeRow =  
      if (m contains node) m(node)
      else TypeRow.empty[Label]
    for {
        tr <- typeRow.addPos(label)
    } yield {
        PosNegTypingAsMap(m + (node -> tr))
    }
  }
  
  override def addNegType(node: Node,label: Label): Try[PosNegTypingAsMap[Node,Label]] = {
    val typeRow =  
      if (m contains node) m(node)
      else TypeRow.empty[Label]
    for {
        tr <- typeRow.addNeg(label)
    } yield {
        PosNegTypingAsMap(m + (node -> tr))
    }
  }
  
  override def getPosTypes(node: Node): Seq[(Label)] = m.get(node).getOrElse(TypeRow.empty).pos.toSeq
  override def getNegTypes(node: Node): Seq[(Label)] = m.get(node).getOrElse(TypeRow.empty).neg.toSeq
  override def getAllTypes(node: Node): (Seq[(Label)],Seq[(Label)]) = (getPosTypes(node),getNegTypes(node))
  override def getLabels(node: Node): Seq[Label] = getPosTypes(node) ++ getNegTypes(node)
  override def asMap = m
  override def combine(other: PosNegTyping[Node,Label]): Try[PosNegTyping[Node,Label]] = {
    val zero : Try[PosNegTyping[Node,Label]] = Success(this)
    other.asMap.foldLeft(zero){
      case (Success(rest),(node,typeRow)) => rest.addTypeRow(node, typeRow)
      case (Failure(e),_) => Failure(e)
    }
  }
  
  def addTypeRow(node: Node, tr: TypeRow[Label]): Try[PosNegTyping[Node,Label]] = {
    //TODO: Simplify the following expression
    if (m contains node) {
      for {
        tr1 <- m(node).combine(tr)
      } yield PosNegTypingAsMap(m = m.updated(node,tr1))
    } else {
     Success(PosNegTypingAsMap(m + (node -> tr))) 
    }
  } 
  
  override def toString: String = {
    def next(rest:String, current: (Node, TypeRow[Label])): String = {
      s"${current._1} -> ${current._2}| " + rest 
    }
    "{" + m.foldLeft("")(next)  + "}"
  }
}

object PosNegTypingAsMap {
  def empty[Node,Label]: PosNegTyping[Node,Label] = {
    PosNegTypingAsMap(Map())
  }
  
}

