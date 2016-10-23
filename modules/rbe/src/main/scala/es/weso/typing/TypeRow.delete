package es.weso.typing

import util._

case class TypeRow[Label](pos: Set[Label], neg: Set[Label]) {
  
  def combine(other: TypeRow[Label]): Try[TypeRow[Label]] = {
    for {
      t1 <- this.addPosSet(other.pos)
      t2 <- t1.addNegSet(other.neg)
    } yield t2
  }

  // TODO: Refactor the following code to DRY
  
  def addPosSet(s: Set[Label]): Try[TypeRow[Label]] = {
    val zero : Try[TypeRow[Label]] = Success(this)
    s.foldLeft(zero) {
      case (Success(rest),label) => rest.addPos(label)
      case (Failure(e),_) => Failure(e)
    }
  }
  
  def addNegSet(s: Set[Label]): Try[TypeRow[Label]] = {
    val zero : Try[TypeRow[Label]] = Success(this)
    s.foldLeft(zero) {
      case (Success(rest),label) => rest.addNeg(label)
      case (Failure(e),_) => Failure(e)
    }

  }
  
  def addPos(label: Label): Try[TypeRow[Label]] = {
    if (negLabels contains label) 
      Failure(TypeFail(s"addPosType: label $label is already in negLabels. Current typeRow: $this"))
    else 
      if (posLabels contains label) {
        Success(this)  // TODO: Should we combine reasonPos?
      } else {
        Success(this.copy(pos = pos + (label)))
      }
  }

    def addNeg(label: Label): Try[TypeRow[Label]] = { 
    if (posLabels contains label) 
      Failure(TypeFail(s"addNegType: label $label is already in posLabels. Current typeRow: $this"))
    else 
      if (negLabels contains label) {
        Success(this)  
      } else {
        Success(this.copy(neg = neg + (label)))
      }
  }
  
  lazy val negLabels : Set[Label] = {
    neg
  }
  
  lazy val posLabels : Set[Label] = {
    pos
  }
  
  override def toString = {
    val sb = new StringBuilder
    if (pos.isEmpty && neg.isEmpty) sb ++= "()"
    else {
      if (!pos.isEmpty) {
       if (pos.size == 1) {
        sb ++= "+" + pos.head.toString
       }
       else {
        sb ++= "+(" + pos.map(_.toString).mkString(",") + ")"        
       }
     }
     if (!neg.isEmpty) { 
      if (neg.size == 1) {
        sb ++= "-" + neg.head.toString
      }
      else {
        sb ++= "-(" + neg.map(_.toString).mkString(",") + ")"        
      }
     }
    }
    sb.toString
  }
}

object TypeRow {
  def empty[Label] = 
    TypeRow(
        pos = Set[Label](), 
        neg = Set[Label]())
        
}
