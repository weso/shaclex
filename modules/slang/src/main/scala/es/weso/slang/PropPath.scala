package es.weso.slang
import es.weso.rdf.nodes._
import es.weso.rdf._

trait PropPath extends Product with Serializable {
/*  private def showSet[A](vs: Set[A]): String = vs.size match {
    case 0 => s"{}"
    case 1 => vs.head.toString
    case _ => s"${vs.map(_.toString).mkString(",")}"
  } */

  def reach(n1: RDFNode, n2: RDFNode, rdf: RDFReader): Boolean
}

case class Pred(p: IRI) extends PropPath {
    override def reach(n1: RDFNode, 
                       n2: RDFNode, rdf: RDFReader
                       ): Boolean = 
       rdf.triplesWithSubjectPredicate(n1,p).map(_.obj).contains(n2)
}
case class Inv(p: IRI) extends PropPath {
    override def reach(n1: RDFNode, 
                       n2: RDFNode, rdf: RDFReader
                       ): Boolean = 
       rdf.triplesWithSubjectPredicate(n2,p).map(_.obj).contains(n1)
}
case class Sequ(pp1: PropPath, pp2: PropPath) extends PropPath {
    override def reach(n1: RDFNode, 
                       n2: RDFNode, rdf: RDFReader
                       ): Boolean = 
       false

}
case class Alt(pp1: PropPath, pp2: PropPath) extends PropPath {
    override def reach(n1: RDFNode, 
                       n2: RDFNode, rdf: RDFReader
                       ): Boolean = 
       false

}
case class ZeroOrMore(pp: PropPath) extends PropPath {
    override def reach(n1: RDFNode, 
                       n2: RDFNode, rdf: RDFReader
                       ): Boolean = 
       false

}
case class NoPreds(preds: Set[IRI]) extends PropPath {
    override def reach(n1: RDFNode, 
                       n2: RDFNode, rdf: RDFReader
                       ): Boolean = 
       false

}

object PropPath {
    def oneOrMore(pp: PropPath): PropPath = Sequ(pp, ZeroOrMore(pp))
}