package es.weso.slang

sealed trait Val extends Product with Serializable {
  def combineVal(other: Val): Val
  def isConforming: Boolean = this match {
    case Conforms | Unknown => true
    case _ => false
  }
}
case object Conforms extends Val {
  override def combineVal(other: Val): Val = other match {
    case Conforms | Unknown => this
    case NotConforms | Inconsistent => Inconsistent
  }
}
case object NotConforms extends Val {
  override def combineVal(other: Val): Val = other match {
    case NotConforms | Unknown => this
    case Conforms | Inconsistent => Inconsistent
  }
}
case object Unknown extends Val {
  override def combineVal(other: Val): Val = other
}
case object Inconsistent extends Val {
  override def combineVal(other: Val): Val = Inconsistent
}
