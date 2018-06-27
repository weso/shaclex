package es.weso.shex
import cats._
import es.weso.rdf.nodes.IRI

sealed trait Path {
 def isDirect: Boolean
}
case class Direct(p: IRI) extends Path {
 val isDirect = true
}
case class Inverse(p: IRI) extends Path {
 val isDirect = false

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

