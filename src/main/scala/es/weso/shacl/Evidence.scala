package es.weso.shacl
import es.weso.rdf.nodes._

case class Evidence(ns: NodeShape, msg: String) {
  
  override def toString: String = 
    s"${ns.node} - ${ns.shape.showId}: $msg" 
}


    
