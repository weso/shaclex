package es.weso.typing

import util._

trait PosTyping[Node,Label] {
  def getPosTypes(node: Node): Seq[(Label)]
  def addPosType(node: Node, label: Label): Try[PosNegTyping[Node,Label]]
  def nodes: Seq[Node]
} 
