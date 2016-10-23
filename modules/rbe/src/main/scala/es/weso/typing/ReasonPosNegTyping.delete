package es.weso.typing

import util._

trait ReasonPosNegTyping[Node,Label, ReasonPos, ReasonNeg] {
  def getPosTypesReason(node: Node): Seq[(Label,ReasonPos)]
  def getNegTypesReason(node: Node): Seq[(Label,ReasonNeg)]
  def getAllTypesReason(node: Node): (Seq[(Label,ReasonPos)],Seq[(Label,ReasonNeg)])
  def addPosTypeReason(node: Node, label: Label, reasonPos: ReasonPos): Try[ReasonPosNegTyping[Node,Label,ReasonPos,ReasonNeg]]
  def addNegTypeReason(node: Node, label: Label, reasonNeg: ReasonNeg): Try[ReasonPosNegTyping[Node,Label,ReasonPos,ReasonNeg]]
  def nodes: Seq[Node]
} 

object ReasonPosNegTyping {
  def empty[Node,Label,ReasonPos,ReasonNeg]: ReasonPosNegTyping[Node,Label,ReasonPos,ReasonNeg] = {
    ReasonTypingAsMap(Map())
  }
}
