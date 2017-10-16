package es.weso.rdf.path

import es.weso.rdf.nodes.{ IRI, RDFNode }
import io.circe.{ Json, _ }
import io.circe.syntax._

sealed trait SHACLPath {
  def predicate: Option[IRI]
}
case class PredicatePath(iri: IRI) extends SHACLPath {
  override def predicate: Option[IRI] = Some(iri)
}
case class InversePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class SequencePath(paths: Seq[SHACLPath]) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class AlternativePath(paths: Seq[SHACLPath]) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class ZeroOrMorePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class OneOrMorePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None
}
case class ZeroOrOnePath(path: SHACLPath) extends SHACLPath {
  override def predicate: Option[IRI] = None
}

object SHACLPath {

  implicit val encodePath: Encoder[SHACLPath] = new Encoder[SHACLPath] {
    final def apply(path: SHACLPath): Json = path match {
      case PredicatePath(iri) => Json.fromString(iri.toString)
      case _ => Json.fromString(s"Error encoding SHACLPath. Not implemented yet: $path")
    }
  }

  implicit val deodePath: Decoder[SHACLPath] = Decoder.instance { c =>
    c.as[String].flatMap(_ match {
      case s => RDFNode.fromString(s).fold(
        s => Left(DecodingFailure(s, Nil)),
        node => node match {
          case iri: IRI => Right(PredicatePath(iri))
          case _ => Left(DecodingFailure(s"Unsupported path decoding of node $node", Nil))
        })
    })
  }

}
