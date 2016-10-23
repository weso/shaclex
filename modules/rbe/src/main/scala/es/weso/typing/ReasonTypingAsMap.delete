package es.weso.typing

import util._

case class ReasonTypingAsMap[Node, Label, ReasonPos, ReasonNeg](
    m: Map[Node,ReasonTypeRow[Label,ReasonPos,ReasonNeg]]
    ) extends ReasonPosNegTyping[Node,Label,ReasonPos,ReasonNeg] {
  
  override def nodes = m.keys.toSeq

  override def addPosTypeReason(n: Node, label: Label, reason: ReasonPos) = {
    val typeRow = 
      if (m contains n) m(n)
      else ReasonTypeRow.empty[Label,ReasonPos,ReasonNeg]
    for {
        tr <- typeRow.addPos(label,reason)
    } yield {
        ReasonTypingAsMap(m + (n -> tr))
    }
  }
  
  override def addNegTypeReason(n: Node, label: Label, reason: ReasonNeg) = {
    val typeRow = 
      if (m contains n) m(n)
      else ReasonTypeRow.empty[Label,ReasonPos,ReasonNeg]
    
    for {
        tr <- typeRow.addNeg(label,reason)
    } yield {
        ReasonTypingAsMap(m + (n -> tr))
    }
  }

  override def getPosTypesReason(node: Node): Seq[(Label,ReasonPos)] = {
    m.get(node).getOrElse(ReasonTypeRow.empty).pos.toSeq
  }
  
  override def getNegTypesReason(node: Node): Seq[(Label,ReasonNeg)] = {
    m.get(node).getOrElse(ReasonTypeRow.empty).neg.toSeq
  }
  override def getAllTypesReason(node: Node): (Seq[(Label,ReasonPos)],Seq[(Label,ReasonNeg)]) = {
    (getPosTypesReason(node),getNegTypesReason(node))
  }
  
  override def toString: String = {
    "Typing:" + m.toString
  }

}
