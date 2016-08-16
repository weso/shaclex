package es.weso.shacl
import es.weso.rdf.nodes._

case class Evidences(m: Map[RDFNode,Map[Shape,String]]) extends AnyVal {
  def addEvidence(ns: NodeShape, msg:String): Evidences = {
    val node = ns.node
    val shape = ns.shape
    Evidences(m.updated(node,
        if (m.get(node).isDefined) {
          val shapesMap = m(node)
          shapesMap.updated(shape,msg)
        } else {
          Map(shape -> msg)
        }))
  }
  
  override def toString: String = 
    m.map{ case (node,ms) => 
      s"$node - ${ms.map{case (shape,msg) => s"${shape.showId} - $msg"}.mkString("\n  ")}"}.
    mkString("\n") 
}

object Evidences {
  def initial = Evidences(Map())
}

    
