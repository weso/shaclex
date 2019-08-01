package es.weso.shex
import cats._
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.{IRI, RDFNode}

sealed trait Path {
 def isDirect: Boolean
 def pred: IRI

 override def toString: String = Path.showPath.show(this)

 def getValues(node: RDFNode, rdf: RDFReader): Either[String, Set[RDFNode]]
}

case class Direct(pred: IRI) extends Path {
 val isDirect = true

  override def getValues(node: RDFNode, rdf: RDFReader): Either[String, Set[RDFNode]] =
    rdf.triplesWithSubjectPredicate(node, pred).map(_.map(_.obj))

}

case class Inverse(pred: IRI) extends Path {
 val isDirect = false

 override def getValues(node: RDFNode, rdf: RDFReader): Either[String, Set[RDFNode]] =
  rdf.triplesWithPredicateObject(pred, node).map(_.map(_.subj))

}

object Path {

  implicit def showPath: Show[Path] = new Show[Path] {
    override def show(x: Path): String = x match {
      case Direct(iri) => iri.toString
      case Inverse(iri) => s"^${iri.toString}"
   }
  }

  implicit def orderingPath: Ordering[Path] = new Ordering[Path] {
    override def compare(x1: Path, x2: Path): Int = {
      x1 match {
        case Direct(p1) =>
          x2 match {
            case Direct(p2) => Ordering[String].compare(p1.str, p2.str)
            case Inverse(_) => 1
          }
        case Inverse(p1) => {
          x2 match {
            case Direct(_) => -1
            case Inverse(p2) => Ordering[String].compare(p1.str, p2.str)
          }
        }
      }
    }
  }

}
// TODO: Handle sequence and alternative paths

