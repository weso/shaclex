package es.weso.typing
import util._

case class ReasonTypeRow[Label,ReasonPos,ReasonNeg](
    pos: Set[(Label,ReasonPos)],
    neg: Set[(Label,ReasonNeg)]) {
  
  def addPos(label : Label, reasonPos: ReasonPos): Try[ReasonTypeRow[Label,ReasonPos,ReasonNeg]] = { 
    if (negLabels contains label) 
      Failure(TypeFail(s"addPos: label $label is already in negLabels. Current typeRow: $this"))
    else 
      if (posLabels contains label) {
        Success(this)  // TODO: Should we combine reasonPos?
      } else {
        Success(this.copy(pos = pos + ((label,reasonPos))))
      }
  }
  
  def addNeg(label: Label,reasonNeg: ReasonNeg): Try[ReasonTypeRow[Label,ReasonPos,ReasonNeg]] = { 
    if (posLabels contains label) 
      Failure(TypeFail(s"addNegType: label $label is already in posLabels. Current typeRow: $this"))
    else 
      if (negLabels contains label) {
        Success(this)  // TODO: Should we combine reasonPos?
      } else {
        Success(this.copy(neg = neg + ((label,reasonNeg))))
      }
  }
  
  lazy val negLabels : Set[Label] = {
    neg.map(_._1)
  }
  
  lazy val posLabels : Set[Label] = {
    pos.map(_._1)
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

object ReasonTypeRow {
  def empty[Label,ReasonPos,ReasonNeg] = 
    ReasonTypeRow(
        pos = Set[(Label,ReasonPos)](), 
        neg = Set[(Label,ReasonNeg)]())
        
}
