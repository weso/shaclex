package es.weso.typing

import util._

trait PosNegTyping[Node,Label] extends PosTyping[Node,Label] {
  def getNegTypes(node: Node): Seq[(Label)]
  def getLabels(node: Node): Seq[Label]
  def getAllTypes(node: Node): (Seq[(Label)],Seq[(Label)])
  def addNegType(node: Node, label: Label): Try[PosNegTyping[Node,Label]]
  def combine(other: PosNegTyping[Node,Label]): Try[PosNegTyping[Node,Label]]
  def addTypeRow(node: Node, tr: TypeRow[Label]): Try[PosNegTyping[Node,Label]]
  def asMap: Map[Node,TypeRow[Label]]
} 
object PosNegTyping {
  def empty[Node,Label] = PosNegTypingAsMap[Node,Label](Map())
  
  def fromPosMap[Node,Label](m: Map[Node,Set[Label]]) =
    PosNegTypingAsMap(m.mapValues(labels => TypeRow(labels, Set())))
    
  def fromPosNegMap[Node,Label](m: Map[Node,(Set[Label],Set[Label])]) =
    PosNegTypingAsMap(m.mapValues(pair => TypeRow(pair._1, pair._2)))
}
