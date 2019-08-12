package es.weso.shacl

import es.weso.rdf.nodes.{IRI, RDFNode}

sealed abstract class Target {
  def toTargetNode: Option[TargetNode] = this match {
    case tn: TargetNode => Some(tn)
    case _ => None
  }
  def toTargetClass: Option[TargetClass] = this match {
    case tc: TargetClass => Some(tc)
    case _ => None
  }
  def toTargetSubjectsOf: Option[TargetSubjectsOf] = this match {
    case t: TargetSubjectsOf => Some(t)
    case _ => None
  }
  def toTargetObjectsOf: Option[TargetObjectsOf] = this match {
    case t: TargetObjectsOf => Some(t)
    case _ => None
  }
}
case class TargetNode(node: RDFNode) extends Target
case class TargetClass(node: RDFNode) extends Target
case class TargetSubjectsOf(pred: IRI) extends Target
case class TargetObjectsOf(pred: IRI) extends Target
