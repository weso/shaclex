package es.weso.rdf.path

import cats._
import cats.implicits._
import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.rdf.nodes.IRI.iriShow
import io.circe.{Json, _}

sealed trait SHACLPath {
  def predicate: Option[IRI]

  def relativize(base: IRI): SHACLPath

}
case class PredicatePath(iri: IRI) extends SHACLPath {
  override def predicate: Option[IRI] = Some(iri)
  override def relativize(base: IRI): SHACLPath =
    PredicatePath(iri.relativizeIRI(base))

}
case class InversePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None

  override def relativize(base: IRI): SHACLPath =
    InversePath(path.relativize(base))
}
case class SequencePath(paths: Seq[SHACLPath]) extends SHACLPath {
  override def predicate: Option[IRI] = None

  override def relativize(base: IRI): SHACLPath =
    SequencePath(paths.map(_.relativize(base)))
}

case class AlternativePath(paths: Seq[SHACLPath]) extends SHACLPath {
  override def predicate: Option[IRI] = None

  override def relativize(base: IRI): SHACLPath =
    AlternativePath(paths.map(_.relativize(base)))

}

case class ZeroOrMorePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None

  override def relativize(base: IRI): SHACLPath =
    ZeroOrMorePath(path.relativize(base))

}

case class OneOrMorePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None

  override def relativize(base: IRI): SHACLPath =
    OneOrMorePath(path.relativize(base))
}

case class ZeroOrOnePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None

  override def relativize(base: IRI): SHACLPath =
    ZeroOrOnePath(path.relativize(base))

}

object SHACLPath {

  implicit val encodePath: Encoder[SHACLPath] = new Encoder[SHACLPath] {
    final def apply(path: SHACLPath): Json = path match {
      case PredicatePath(iri) => Json.fromString(iri.toString)
      case _ => Json.fromString(s"Error encoding SHACLPath. Not implemented yet: $path")
    }
  }

  implicit val decodePath: Decoder[SHACLPath] = Decoder.instance { c =>
    c.as[String].flatMap(_ match {
      case s => RDFNode.fromString(s).fold(
        s => Left(DecodingFailure(s, Nil)),
        node => node match {
          case iri: IRI => Right(PredicatePath(iri))
          case _ => Left(DecodingFailure(s"Unsupported path decoding of node $node", Nil))
        })
    })
  }

  implicit val shaclPathShow: Show[SHACLPath] = new Show[SHACLPath] {
    def show(path: SHACLPath): String = {
      path match {
        case PredicatePath(pred) => pred.show
        case InversePath(p)      => "^ " + p.show
        case SequencePath(ps)    => ps.map(_.show).mkString(" / ")
        case AlternativePath(ps) => ps.map(_.show).mkString(" | ")
        case ZeroOrMorePath(p)   => p.show + "* "
        case OneOrMorePath(p)    => p.show + "+ "
        case ZeroOrOnePath(p)    => p.show + "? "
      }
    }
  }

}
