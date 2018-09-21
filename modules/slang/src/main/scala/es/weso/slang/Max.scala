package es.weso.slang

sealed trait Max                extends Product with Serializable {
  override def toString: String = this match {
    case IntMax(n) => n.toString
    case Star => "*"
  }
}
final case object Star          extends Max
final case class IntMax(n: Int) extends Max
