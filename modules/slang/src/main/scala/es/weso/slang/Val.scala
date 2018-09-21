package es.weso.slang

sealed trait Val extends Product with Serializable {
  def combineVal(other: Val): Val = (this,other) match {
    case (Conforms,Conforms) => Conforms
    case (Conforms,NotConforms) => Inconsistent
    case (Conforms,Unknown) => Conforms
    case (NotConforms,Conforms) => Inconsistent
    case (NotConforms,NotConforms) => NotConforms
    case (NotConforms,Unknown) => NotConforms
    case (Unknown,Conforms) => Conforms
    case (Unknown,NotConforms) => NotConforms
    case (Unknown,Unknown) => Unknown
    case (Inconsistent,_) => Inconsistent
    case (_,Inconsistent) => Inconsistent
  }
  def isConforming: Boolean = this match {
    case Conforms | Unknown => true
    case _ => false
  }
  override def toString = this match {
    case Conforms => "+"
    case NotConforms => "-"
    case Unknown => "?"
    case Inconsistent => "!!"
  }
}
case object Conforms extends Val
case object NotConforms extends Val
case object Unknown extends Val
case object Inconsistent extends Val

object Val {
  def and(v1: Val, v2: Val): Val = (v1,v2) match {
    case (Conforms,Conforms) => Conforms
    case (Conforms, NotConforms) => NotConforms
    case (Conforms, Unknown) => Unknown
    case (NotConforms, _) => NotConforms
    case (Unknown, Conforms) => Unknown
    case (Unknown, Unknown) => Unknown
    case (Unknown, NotConforms) => NotConforms
    case (Inconsistent,_) => Inconsistent
    case (_,Inconsistent) => Inconsistent
  }
}