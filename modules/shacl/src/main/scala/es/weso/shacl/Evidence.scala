package es.weso.shacl

case class Evidences(ls: List[(NodeShape,String)]) {
  def addEvidence(ns: NodeShape, msg:String): Evidences = {
    Evidences((ns,msg) :: ls)
  }
  
  override def toString: String = 
    ls.map{ case (ns,msg) => s"${ns.node} - ${ns.shape.showId}: $msg" }.mkString("\n") 
}

object Evidences {
  def initial = Evidences(List())
}

    
