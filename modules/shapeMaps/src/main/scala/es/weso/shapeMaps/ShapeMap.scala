package es.weso.shapeMaps

import es.weso.rdf.nodes._
import cats._
import cats.implicits._
import es.weso.rdf.{PrefixMap, RDFReader}
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import ShapeMap._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.path._
import scala.io.Source
import scala.util.Try

abstract class ShapeMap {
  val associations: List[Association]
  val nodesPrefixMap: PrefixMap
  val shapesPrefixMap: PrefixMap

  def add(node: RDFNode, label: ShapeMapLabel): Either[String,ShapeMap] =
    addAssociation(Association(RDFNodeSelector(node),label))

  def addAssociation(a: Association): Either[String, ShapeMap]

  def toJson: Json = {
    this.asJson
  }

  override def toString = Show[ShapeMap].show(this)

  def serialize(format: String): String = {
    format.toUpperCase match {
      case "JSON" => this.toJson.spaces2
      case "COMPACT" => this.toString
    }
  }

}

object ShapeMap {

  def empty: ShapeMap = FixedShapeMap.empty

  def fromURI(uri: String,
              format: String,
              base: Option[String],
              nodesPrefixMap: PrefixMap,
              shapesPrefixMap: PrefixMap): Either[String, ShapeMap] = {
   val t = Try {
      val contents = Source.fromURL(uri).mkString
      val either: Either[String, ShapeMap] = {
        fromString(contents, format, base, nodesPrefixMap, shapesPrefixMap)
      }
      either
    }
   t.fold(e => Left(s"Exception obtaining URI contents. URI = ${uri}. Error: ${e.getLocalizedMessage}"),
     identity
   )
  }
  def fromString(str: String,
                 format: String,
                 base: Option[String] = None,
                 nodesPrefixMap: PrefixMap = PrefixMap.empty,
                 shapesPrefixMap: PrefixMap = PrefixMap.empty
                ): Either[String,ShapeMap] =
    format.toUpperCase match {
     case "JSON" => fromJson(str)
     case "COMPACT" => {
       fromCompact(str,base,nodesPrefixMap,shapesPrefixMap)
     }
     case _ => Left(s"Unknown format for shapeMap")
   }

  def fromCompact(
    str: String,
    base: Option[String] = None,
    nodesPrefixMap: PrefixMap = PrefixMap.empty,
    shapesPrefixMap: PrefixMap = PrefixMap.empty): Either[String, ShapeMap] = {
    if (str.isEmpty) Right(ShapeMap.empty)
    else Parser.parse(str, base, nodesPrefixMap, shapesPrefixMap)
  }

  def fromJson(jsonStr: String): Either[String, ShapeMap] = {
    decode[ShapeMap](jsonStr).leftMap(_.getMessage)
  }

  def parseResultMap(
    str: String,
    base: Option[String],
    rdf: RDFReader,
    shapesPrefixMap: PrefixMap = PrefixMap.empty): Either[String, ResultShapeMap] = for {
    queryMap <- {
      Parser.parse(str, base, rdf.getPrefixMap, shapesPrefixMap)
    }
    fixMap <- fixShapeMap(queryMap, rdf, rdf.getPrefixMap, shapesPrefixMap)
  } yield ResultShapeMap(fixMap.shapeMap, rdf.getPrefixMap, shapesPrefixMap)

  /**
   * Resolve triple patterns according to an RDF
   */
  def fixShapeMap(
    shapeMap: ShapeMap,
    rdf: RDFReader,
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap): Either[String, FixedShapeMap] = {

    val empty: Either[String, FixedShapeMap] = Right(
      FixedShapeMap.empty.
        addNodesPrefixMap(nodesPrefixMap).
        addShapesPrefixMap(shapesPrefixMap)
    )

    def addNode(a: Association)(
      node: RDFNode,
      current: Either[String, FixedShapeMap]
    ): Either[String, FixedShapeMap] = for {
      fixed <- current
      newShapeMap <- fixed.addAssociation(Association(RDFNodeSelector(node), a.shape))
    } yield newShapeMap

    def combine(a: Association, current: Either[String, FixedShapeMap]): Either[String, FixedShapeMap] = {
      for {
        nodes <- a.node.select(rdf)
        r <- nodes.foldRight(current)(addNode(a))
      } yield r
    }
    shapeMap.associations.foldRight(empty)(combine)
  }

  implicit val encodeShapeMap: Encoder[ShapeMap] = new Encoder[ShapeMap] {
    final def apply(a: ShapeMap): Json = a.associations.asJson
  }

  implicit val showShapeMap: Show[ShapeMap] = new Show[ShapeMap] {

    final def show(a: ShapeMap): String = {

      implicit val showPattern: Show[Pattern] = new Show[Pattern] {
        final def show(n: Pattern): String = {
          n match {
            case NodePattern(node) => a.nodesPrefixMap.qualify(node)
            case WildCard => "_"
            case Focus => "FOCUS"
          }
        }
      }

      /*implicit val showPredicate: Show[IRI] = new Show[IRI] {
        final def show(iri: IRI): String = iri match {
          case `rdf_type` => "a"
          case _ => a.nodesPrefixMap.qualify(iri)
        }
      } */

      implicit val showPath: Show[SHACLPath] = new Show[SHACLPath] {
        final def show(path: SHACLPath): String = path match {
          case PredicatePath(`rdf_type`) => "a"
          case PredicatePath(iri) => a.nodesPrefixMap.qualify(iri)
          case InversePath(path) => s"^${show(path)}"
          case SequencePath(paths) => paths.map(show(_)).mkString("/")
          case AlternativePath(paths) => paths.map(show(_)).mkString("|")
          case OneOrMorePath(path) => s"${show(path)}+"
          case ZeroOrMorePath(path) => s"${show(path)}*"
          case ZeroOrOnePath(path) => s"${show(path)}?"
        }
      }

      implicit val showNodeSelector: Show[NodeSelector] = new Show[NodeSelector] {
        final def show(n: NodeSelector): String = {
          n match {
            case RDFNodeSelector(node) => a.nodesPrefixMap.qualify(node)
            case TriplePattern(sub, path, obj) => s"{${sub.show} ${path.show} ${obj.show}}"
            case SparqlSelector(query) => s"""SPARQL `$query`"""
          }
        }
      }

      implicit val showShapeMapLabel: Show[ShapeMapLabel] = new Show[ShapeMapLabel] {
        final def show(label: ShapeMapLabel): String = label match {
          case IRILabel(iri) => a.shapesPrefixMap.qualify(iri)
          case BNodeLabel(bn) => bn.getLexicalForm
          case Start => "Start"
        }
      }

      implicit val showAssociation: Show[Association] = new Show[Association] {
        final def show(a: Association): String = {
          s"${a.node.show}@${if (a.info.status==NonConformant) "!" else ""}${a.shape.show}"
        }
      }

      a.associations.map(_.show).mkString(",")

    }
  }

  implicit val decodeShapeMap: Decoder[ShapeMap] = Decoder.instance { c =>
    for {
      associations <- c.as[List[Association]]
    } yield QueryShapeMap(associations, PrefixMap.empty, PrefixMap.empty)
  }

  def formats: List[String] = List("COMPACT", "JSON")

  def defaultFormat = formats.head
}
