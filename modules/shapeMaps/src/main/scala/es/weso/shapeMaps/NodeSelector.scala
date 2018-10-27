package es.weso.shapeMaps

import es.weso.rdf.nodes.{IRI, RDFNode}
import es.weso.rdf.path.SHACLPath
import io.circe._
import io.circe.syntax._
import es.weso.json.DecoderUtils._
import es.weso.rdf.{PrefixMap, RDFReader}


abstract class NodeSelector {
  def select(rdf: RDFReader): Either[String,Set[RDFNode]]
}
case class RDFNodeSelector(node: RDFNode) extends NodeSelector {
  override def select(rdf: RDFReader): Either[String,Set[RDFNode]] =
    Right(Set(node))
}

case class TriplePattern(
  subjectPattern: Pattern,
  path: SHACLPath,
  objectPattern: Pattern) extends NodeSelector {
  override def select(rdf: RDFReader): Either[String,Set[RDFNode]] =
    (subjectPattern, path, objectPattern) match {
      case (Focus,p,WildCard) => Right(rdf.nodesWithPath(p).map(_._1))
      case (Focus,p,NodePattern(obj)) => Right(rdf.subjectsWithPath(p,obj))
      case (WildCard,p,Focus) => Right(rdf.nodesWithPath(p).map(_._2))
      case (NodePattern(subj),p,Focus) =>  Right(rdf.objectsWithPath(subj, p))
      case _ => Left(s"Strange triple pattern in node selector: $this")
    }
}

case class SparqlSelector(query: String) extends NodeSelector {
  override def select(rdf: RDFReader): Either[String,Set[RDFNode]] = {
    rdf.querySelect(query) match {
      case Left(str) => Left(str)
      case Right(result) => {
        val zero: Either[String,List[RDFNode]] = Right(List())
        def combine(current: Either[String,List[RDFNode]], next: Map[String,RDFNode]): Either[String,List[RDFNode]] = {
          current match {
            case Left(msg) => Left(msg)
            case Right(nodes) => if (next.size == 1) {
              val resultNode = next.map(_._2).head
              Right(resultNode :: nodes)
            } else {
              Left(s"Result of query has more than one value: $next")
            }
          }
        }
        result.foldLeft(zero)(combine).map(_.toSet)
      }
    }
  }
}

case class GenericSelector(iri: IRI, param: String) extends NodeSelector {
  override def select(rdf: RDFReader): Either[String, Set[RDFNode]] = {
    Left(s"Not implemented GenericSelector($iri, $param)")
  }
}

object NodeSelector {

  def fromString(str: String, base: Option[String], pm: PrefixMap): Either[String,NodeSelector] =
    ParserNodeSelector.parse(str, base, pm)

  implicit val encodeNodeSelector: Encoder[NodeSelector] = new Encoder[NodeSelector] {
    final def apply(nodeSelector: NodeSelector): Json = {
      nodeSelector match {
        case RDFNodeSelector(node) => Json.fromString(node.toString)
        case TriplePattern(subj, path, obj) => {
          Json.fromJsonObject(JsonObject.empty.
            add("subject", subj.asJson).
            add("path", path.asJson).
            add("object", obj.asJson))
        }
        case SparqlSelector(query) =>
          Json.fromJsonObject(JsonObject.empty.add("sparql", Json.fromString(query)))
      }
    }
  }

  implicit lazy val decodeRDFNodeSelector: Decoder[RDFNodeSelector] = Decoder.instance { c => {
    c.as[String].flatMap(s => {
      RDFNode.fromString(s).fold(
      s => {
        println(s"RDF.fromString Error: $s")
        Left(DecodingFailure(s, Nil))
      } ,
      node => Right(RDFNodeSelector(node)))
    })
    }
  }

  implicit lazy val decodeTriplePattern: Decoder[TriplePattern] = Decoder.instance { c =>
    for {
      subj <- fieldDecode[Pattern](c, "subject")
      path <- fieldDecode[SHACLPath](c, "path")
      obj <- fieldDecode[Pattern](c, "object")
    } yield TriplePattern(subj, path, obj)

  }

  implicit lazy val decodeSparql: Decoder[SparqlSelector] = Decoder.instance { c =>
    for {
      query <- fieldDecode[String](c, "sparql")
    } yield SparqlSelector(query)
  }

  implicit lazy val decodeNodeSelector: Decoder[NodeSelector] =
    Decoder[RDFNodeSelector].map(identity).or(
     Decoder[TriplePattern].map(identity).or(
      Decoder[SparqlSelector].map(identity)
    )
   )

}